#include <Lifter/Lifter.hpp>
#include <Optimizer/Optimizer.hpp>

#include <llvm/ADT/SmallSet.h>
#include <llvm/Support/Casting.h>

#include <iostream>
#include <set>
#include <sstream>
#include <stack>
#include <type_traits>

// Binary exploration component: <- BasicBlockStream
// (Binary +) EntryPoint -> BasicBlock

// Lifter component: <- LifterStream
// BasicBlock -> LLVM IR function

// Unknown component?
// BasicBlock + LLVM IR -> Children (maybe?)

// CFGConstructor:
// 1. Ask BasicBlockStream to give BasicBlock for EntryPoint X
// 2. Ask LifterStream to give LLVM IR for BasicBlock
// 3. Do magic (optimize + detect children)
// 4. Go to 1 for each child?
// 5. Make some output from reconstructed CFG

void parse(const std::string &filename) {}

// Default constructor

Lifter::Lifter(std::shared_ptr<llvm::Module> HelpersModule) : Module(std::move(HelpersModule)) {
  // 1. Load the LLVM helpers
  this->LoadHelpersInterface();
}

// Load the LLVM helpers

void Lifter::LoadHelpersInterface() {
  this->HelperFunction = this->Module->getFunction("HelperFunction");
  if (!this->HelperFunction)
    throw std::invalid_argument("Module does not contain 'HelperFunction'");

  this->HelperSlicePC = this->Module->getFunction("HelperSlicePC");
  if (!this->HelperSlicePC)
    throw std::invalid_argument("Module does not contain 'HelperSlicePC'");

  this->HelperStubEmpty = this->Module->getFunction("HelperStubEmpty");
  if (!this->HelperStubEmpty)
    throw std::invalid_argument("Module does not contain 'HelperStubEmpty'");

  this->HelperKeepPC = this->Module->getFunction("HelperKeepPC");
  if (!this->HelperKeepPC)
    throw std::invalid_argument("Module does not contain 'HelperKeepPC'");

  // The optimizer adds this attribute to the empty function, but it causes incorrect assumptions
  // for the optimizer
  this->HelperStubEmpty->removeFnAttr(llvm::Attribute::AttrKind::ReadNone);

  this->UndefVariable = this->Module->getGlobalVariable("__undef");
  if (!this->UndefVariable)
    throw std::invalid_argument("Module does not contain '__undef'");

  {
    this->HelperStub = this->Module->getFunction("HelperStub");
    if (!this->HelperStub)
      throw std::invalid_argument("Module does not contain 'HelperStub'");
    if (this->HelperStub->getFunctionType() != this->HelperStubEmpty->getFunctionType())
      throw std::invalid_argument("HelperStub != HelperStubEmpty");
    llvm::SmallSet<llvm::Function *, 2> ExpectedCallers;
    ExpectedCallers.insert(this->HelperFunction);
    ExpectedCallers.insert(this->HelperSlicePC);
    bool functionMatches = true;
    for (const auto &user : this->HelperStub->users()) {
      if (auto *callerInst = llvm::dyn_cast<llvm::CallInst>(user)) {
        if (auto *callerFunction = GetParentFunction(callerInst)) {
          if (!ExpectedCallers.contains(callerFunction)) {
            functionMatches = false;
            break;
          }
        }
      }

      if (!functionMatches)
        throw std::invalid_argument("HelperStub caller is not HelperFunction");
    }
  }

  for (const auto &global : this->Module->globals()) {
    auto globalName = global.getName();
    if (globalName.startswith("SEM_")) {
      if (global.isConstant()) {
        if (global.getType()->isPointerTy()) {
          if (auto *initializer = global.getInitializer()) {
            auto fn = Module->getFunction(initializer->getName());
            if (fn) {
              auto semanticName = globalName.str().substr(4);
              Semantics.emplace(semanticName, fn);
            } else {
              throw std::runtime_error("SEM_ initializer not a function");
            }
          } else {
            throw std::runtime_error("SEM_ no initializer");
          }
        } else {
          throw std::runtime_error("SEM_ not pointer");
        }
      } else {
        throw std::runtime_error("SEM_ not constant");
      }
    }
  }

  // Make the llvm.x86.rdtsc 'readnone'
  auto *__rdtsc = this->Module->getFunction("llvm.x86.rdtsc");
  if (__rdtsc)
    __rdtsc->addFnAttr(llvm::Attribute::ReadNone);
}

// Duplicate a function

llvm::Function *Lifter::DuplicateFunction(llvm::Function *F, const std::string &FN) {
  // 1. Get the original function type
  auto *FT = F->getFunctionType();
  // 2. Create a new empty function
  auto *NF = llvm::Function::Create(FT, llvm::GlobalValue::InternalLinkage, FN, *F->getParent());
  // 3. Clone the function body
  CloneFunctionInto(F, NF);
  // 4. Return the cloned function
  return NF;
}

llvm::Function *Lifter::GetParentFunction(llvm::Instruction *inst) {
  auto *parent = inst->getParent();
  if (!parent)
    return nullptr;
  return llvm::dyn_cast<llvm::Function>(parent->getParent());
}

const llvm::Function *Lifter::GetParentFunction(const llvm::Instruction *inst) {
  auto *parent = inst->getParent();
  if (!parent)
    return nullptr;
  return llvm::dyn_cast<llvm::Function>(parent->getParent());
}

// Dump a function

void Lifter::DumpFunction(const std::string &Name) const {
  // 1. Try to find the function
  auto *F = this->Module->getFunction(Name);
  // 2. Dump it, if found
  if (F) {
    F->dump();
  } else {
    llvm::outs() << "DumpFunction: " << Name << " not found in the module!"
                 << "\n";
  }
}

struct ArgumentHelper {
private:
  std::vector<std::pair<std::string, llvm::Argument *>> Arguments;
  std::map<std::string, size_t> ArgumentMap;

public:
  ArgumentHelper(llvm::Function *fn) {
    for (auto &arg : fn->args()) {
      auto name = arg.getName().str();
      ArgumentMap.emplace(name, Arguments.size());
      Arguments.emplace_back(name, &arg);
    }
  }

  bool contains(const std::string &name) const { return ArgumentMap.count(name) != 0; }

  llvm::Argument *operator[](const std::string &key) const {
    return Arguments.at(ArgumentMap.at(key)).second;
  }

  auto begin() { return Arguments.begin(); }
  auto end() { return Arguments.end(); }
};

void Lifter::LiftInstruction(const whitepeacock::Instruction &instruction, llvm::BasicBlock *insertAtEnd, 
  ArgumentHelper &argumentHelper) {
  // std::cout << instruction.toString() << std::endl;
  auto semanticItr = Semantics.find(instruction.Mnemonic);
  if (semanticItr == Semantics.end()) {
    throw std::invalid_argument("Semantic '" + instruction.Mnemonic + "' not found (" +
                                instruction.ErrorContext + ")");
  }

  ArgumentHelper semanticArgumentHelper(semanticItr->second);

  // Validate arguments
  for (const auto &operand : instruction.Operands) {
    if (operand.Type == whitepeacock::OperandType::Var) {
      auto varName = operand.Var;
      auto indexIdx = varName.rfind('[');
      if (indexIdx != std::string::npos)
        varName.resize(indexIdx);
      if (semanticArgumentHelper.contains(varName)) {
        // You cannot name arguments to semantics the same as the implicit arguments in the
        // helpers
        throw std::invalid_argument(
            "Semantic '" + instruction.Mnemonic + "' has implicit arguments with the same name '" +
            varName + "' as used in the input file (" + instruction.ErrorContext + ")");
      }
    }
  }

  std::vector<llvm::Value *> callArgs;
  std::vector<llvm::Type *> explicitArgs;

  auto generateArgumentLoad = [&argumentHelper, &callArgs, insertAtEnd, &instruction](
                                  std::string argumentName, llvm::Type *semanticArgumentType,
                                  bool isImplicitArgument) {
    // std::cout << "generateArgumentLoad: " << argumentName << std::endl;
    ssize_t index = -1;
    if (argumentName.back() == ']') {
      // Little bit ugly hack to parse varName[index]
      std::stringstream argSs(argumentName);
      std::string varName;
      std::getline(argSs, varName, '[');
      argSs >> index;
      // std::cout << varName << '[' << index << ']' << std::endl;
      argumentName = varName;
    }

    if (!argumentHelper.contains(argumentName)) {
      std::string argumentType = isImplicitArgument ? "Implicit argument" : "Explicit argument";
      throw std::invalid_argument(argumentType + " named '" + argumentName +
                                  "' missing from HelperStub (" + instruction.ErrorContext + ")");
    }

    auto *argument = argumentHelper[argumentName];

    if (index != -1) {
      // Load from array
      if (semanticArgumentType->isPointerTy()) {
        std::vector<llvm::Value *> indices = {
            llvm::ConstantInt::get(llvm::Type::getInt64Ty(insertAtEnd->getContext()), index),
        };

        auto *elementPtr =
            llvm::GetElementPtrInst::CreateInBounds(argument, indices, "", insertAtEnd);
        callArgs.push_back(elementPtr);
      } else {
        // TODO: make this code more generic or enforce a specific "shape" on the structures

        // Let's calculate the pointer to the element
        std::vector<llvm::Value *> indices = {
            llvm::ConstantInt::get(llvm::Type::getInt64Ty(insertAtEnd->getContext()), index)};
        auto *elementPtr =
            llvm::GetElementPtrInst::CreateInBounds(argument, indices, "", insertAtEnd);

        // Offset the pointer until needed (for the nested structures)
        while (elementPtr->getResultElementType() != semanticArgumentType) {
          indices.push_back(
              llvm::ConstantInt::get(llvm::Type::getInt32Ty(insertAtEnd->getContext()), 0));
          elementPtr->eraseFromParent();
          elementPtr = llvm::GetElementPtrInst::CreateInBounds(argument, indices, "", insertAtEnd);
        }

        // Load the value from the pointer
        callArgs.push_back(new llvm::LoadInst(semanticArgumentType, elementPtr, "", insertAtEnd));
      }
    } else if (argument->getType() == semanticArgumentType) {
      // Directly pass the argument from the cloned helper function to the semantic
      callArgs.push_back(argument);
    } else if (!semanticArgumentType->isPointerTy() && argument->getType()->isPointerTy() &&
               argument->getType()->getPointerElementType() == semanticArgumentType) {
      // Generate a load and pass the loaded value to the helper function
      callArgs.push_back(new llvm::LoadInst(argument->getType()->getPointerElementType(), argument,
                                            "", insertAtEnd));
    } else {
      std::string argumentType = isImplicitArgument ? "implicit argument" : "explicit argument";
      throw std::invalid_argument("Semantic " + argumentType +
                                  " pointer expectation mismatch for " + instruction.Mnemonic +
                                  " (" + instruction.ErrorContext + ")");
    }
  };

  // Generate implicit arguments
  for (const auto &[argumentName, semArgument] : semanticArgumentHelper) {
    if (!argumentHelper.contains(argumentName)) {
      // Store semantic argument types for later use
      explicitArgs.push_back(semArgument->getType());
    } else {
      generateArgumentLoad(argumentName, semArgument->getType(), true);
    }
  }

  // Check that the total amount of arguments is correct
  if (explicitArgs.size() != instruction.Operands.size()) {
    throw std::invalid_argument("Incorrect amount of arguments for semantic (" +
                                instruction.ErrorContext + ")");
  }

  // Generate (explicit) operand arguments
  for (size_t i = 0; i < instruction.Operands.size(); i++) {
    const auto &operand = instruction.Operands[i];
    auto operandType = explicitArgs[i];
    switch (operand.Type) {
    case whitepeacock::OperandType::Imm: {
      if (!operandType->isIntegerTy()) {
        throw std::invalid_argument(
            "Cannot pass constant to semantic argument that's not an integer " +
            instruction.Mnemonic + " (" + instruction.ErrorContext + ")");
      }
      callArgs.push_back(llvm::ConstantInt::get(operandType, operand.Imm));
    } break;
    case whitepeacock::OperandType::Var: {
      generateArgumentLoad(operand.Var, operandType, false);
    } break;
    default:
      throw std::exception();
    }
  }

  // Generate call instruction
  llvm::CallInst::Create(semanticItr->second, callArgs, "", insertAtEnd);
}

void Lifter::OptimizeFunction(llvm::Function *F, OptimizationGuide &Guide, bool Undefine, 
  bool Clean, bool Strip) const {
  // inline all the functions
  while (inlineRemillFunctions(F)) {
  }
  // optimize the function
  optimizeFunction(F, Guide);
  // replace __undef usages and optimize again
  if (Undefine) {
    for (auto *user : UndefVariable->users()) {
      if (auto *callerInst = llvm::dyn_cast<llvm::LoadInst>(user)) {
        if (auto parentFn = GetParentFunction(callerInst)) {
          if (parentFn == F) {
            auto *undefValue = llvm::UndefValue::get(callerInst->getType());
            callerInst->replaceAllUsesWith(undefValue);
          }
        }
      }
    }
    optimizeFunction(F, Guide);
  }
  // Remove some useless functions
  if (Clean) {
    auto *ExperimentalNoAliasScope =
        F->getParent()->getFunction("llvm.experimental.noalias.scope.decl");
    if (ExperimentalNoAliasScope) {
      for (auto *Use : ExperimentalNoAliasScope->users()) {
        if (auto *C = llvm::dyn_cast<llvm::CallInst>(Use)) {
          if (C->getFunction() == F) {
            C->eraseFromParent();
          }
        }
      }
    }
  }
  // Strip the names from the instructions and basic blocks
  if (Strip) {
    for (auto &BB : *F) {
      BB.setName("");
      for (auto &I : BB) {
        if (I.hasName())
          I.setName("");
      }
    }
  }
}

std::string to_string_hex(uint64_t v) {
  char buf[64];
  sprintf(buf, "%" PRIx64, v);
  return buf;
}

void doDump(llvm::Value *v) { v->dump(); }

llvm::Function *Lifter::LiftBasicBlock(const whitepeacock::BasicBlock &basicBlock) {
  auto *llvmFn = DuplicateFunction(this->HelperStubEmpty,
                                   "BasicBlock_" + to_string_hex(basicBlock.EntryPoint));
  ArgumentHelper argumentHelper(llvmFn);

  // Delete original 'entry' basic block
  llvmFn->getEntryBlock().eraseFromParent();
  auto *llvmBB = llvm::BasicBlock::Create(llvmFn->getContext(), "liftedEntry", llvmFn);
  for (const auto &instruction : basicBlock.Instructions) {
    LiftInstruction(instruction, llvmBB, argumentHelper);
  }

  // Identify the instruction pointer
  llvm::Argument *PCPtr = nullptr;
  for (auto &arg : llvmFn->args()) {
    if (arg.getName() == "vip") {
      PCPtr = &arg;
      break;
    }
  }

  if (!PCPtr)
    throw std::invalid_argument("HelperStubEmpty is expected to have a 'vip' argument!");

  // Return the updated program counter
  auto *PC = new llvm::LoadInst(PCPtr->getType()->getPointerElementType(), PCPtr, "", llvmBB);
  llvm::ReturnInst::Create(llvmFn->getContext(), PC, llvmBB);

  // Return the crafted function
  return llvmFn;
}

llvm::Function *
Lifter::GetControlFlowGraph(uint64_t EntryPoint, uint64_t Target, 
  const std::unordered_map<uint64_t, whitepeacock::BasicBlock> &BasicBlocks,
  bool IsPartialCFG, bool Debug) const {
  size_t HelperCount = 0;
  // 1. Duplicate the empty stub
  auto *stubFn = DuplicateFunction(this->HelperStubEmpty, "ControlFlowGraphStub");
  // 2. Fetch the LLVM context
  auto &Context = stubFn->getContext();
  // 3. Collect the arguments
  std::vector<llvm::Value *> Arguments;
  for (auto &arg : stubFn->args())
    Arguments.push_back(&arg);
  // 4. Create a fresh entry block
  stubFn->getEntryBlock().eraseFromParent();
  auto *entryBasicBlock = llvm::BasicBlock::Create(Context, "Entry", stubFn);
  // 5. Create the empty basic blocks
  std::unordered_map<uint64_t, llvm::BasicBlock *> IRBasicBlocks;
  std::vector<llvm::CallInst *> Calls;
  std::stack<uint64_t> Worklist;
  llvm::SmallSet<uint64_t, 8> Explored;
  Worklist.push(EntryPoint);
  do {
    // Fetch the current address
    uint64_t address = Worklist.top();
    Worklist.pop();
    // Skip if known
    if (Explored.contains(address))
      continue;
    Explored.insert(address);
    // Create a new basic block
    auto *irBasicBlock = llvm::BasicBlock::Create(Context, "BB_" + to_string_hex(address), stubFn);
    // Save the basic block
    IRBasicBlocks[address] = irBasicBlock;
    // Skip the block if missing
    if (!BasicBlocks.count(address))
      continue;
    // Add the children to the worklist
    for (uint64_t child : BasicBlocks.at(address).Children)
      Worklist.push(child);
  } while (!Worklist.empty());
  // 6. Populate the basic blocks
  Explored.clear();
  Worklist.push(EntryPoint);
  do {
    // Fetch the current address
    uint64_t address = Worklist.top();
    Worklist.pop();
    // Skip if known
    if (Explored.contains(address))
      continue;
    Explored.insert(address);
    // Fetch the LLVM basic block
    auto *irBasicBlock = IRBasicBlocks.at(address);
    // Populate the basic block
    if (!BasicBlocks.count(address)) {
      // Unreachable block, consider it as an exit
      llvm::ReturnInst::Create(
          Context, llvm::ConstantInt::get(stubFn->getFunctionType()->getReturnType(), 0xDEADEAD),
          irBasicBlock);
    } else {
      // Fetch the basic block
      auto &basicBlock = BasicBlocks.at(address);
      // Insert the basic block body
      auto *PC = llvm::CallInst::Create(basicBlock.LiftedCode, Arguments, "pc", irBasicBlock);
      // Save the basic block body call
      Calls.push_back(PC);
      // Check if we reached the target
      if ((Target == address) && IsPartialCFG) {
        std::vector<llvm::Value *> Arguments{PC, PC};
        PC = llvm::CallInst::Create(this->HelperKeepPC, Arguments, "", irBasicBlock);
      }
      // Insert the basic block terminator
      std::vector<uint64_t> children(basicBlock.Children.begin(), basicBlock.Children.end());
      switch (children.size()) {
      case 0: {
        // Is this the target basic block?
        if (Target == address) {
          // Exit block, keep the PC in the final CFG
          if (!IsPartialCFG) {
            // Get the stack pointer argument and the RAM array
            auto *RAM = stubFn->getParent()->getGlobalVariable("RAM");
            llvm::Value *spArg = nullptr;
            for (auto *arg : Arguments) {
              if (arg->getName() == "rsp" || arg->getName() == "esp") {
                spArg = arg;
                break;
              }
            }
            if (!spArg || !RAM) {
              std::stringstream ss;
              ss << __func__;
              ss << ": unable to identify the stack pointer argument!";
              llvm::report_fatal_error(ss.str());
            }
            // Load the latest pushed value
            auto *sp = new llvm::LoadInst(spArg->getType()->getPointerElementType(), spArg, "", irBasicBlock);
            std::vector<llvm::Value *> indices{
              llvm::ConstantInt::get(llvm::Type::getInt32Ty(stubFn->getContext()), 0),
              sp
            };
            auto *gep = llvm::GetElementPtrInst::CreateInBounds(RAM, indices, "", irBasicBlock);
            auto *bc = new llvm::BitCastInst(gep, spArg->getType(), "", irBasicBlock);
            auto *ret = new llvm::LoadInst(spArg->getType()->getPointerElementType(), bc, "", irBasicBlock);
            // Generate the call to retain the pointers
            std::vector<llvm::Value *> Arguments{PC, ret};
            auto *DHUPC = DuplicateFunction(this->HelperKeepPC, "KeepReturnAddress_" + std::to_string(HelperCount++));
            DHUPC->addFnAttr(llvm::Attribute::NoDuplicate);
            DHUPC->addFnAttr(llvm::Attribute::NoInline);
            DHUPC->addFnAttr(llvm::Attribute::NoMerge);
            auto *Res = llvm::CallInst::Create(DHUPC, Arguments, "", irBasicBlock);
            llvm::ReturnInst::Create(Context, Res, irBasicBlock);
          } else {
            // Return the program counter
            llvm::ReturnInst::Create(Context, PC, irBasicBlock);
          }
        } else if (basicBlock.LiftedCode) {
          // Exit block, keep the PC in the final CFG
          if (!IsPartialCFG) {
            // Get the stack pointer argument and the RAM array
            auto *RAM = stubFn->getParent()->getGlobalVariable("RAM");
            llvm::Value *spArg = nullptr;
            for (auto *arg : Arguments) {
              if (arg->getName() == "rsp" || arg->getName() == "esp") {
                spArg = arg;
                break;
              }
            }
            if (!spArg || !RAM) {
              std::stringstream ss;
              ss << __func__;
              ss << ": unable to identify the stack pointer argument!";
              llvm::report_fatal_error(ss.str());
            }
            // Load the latest pushed value
            auto *sp = new llvm::LoadInst(spArg->getType()->getPointerElementType(), spArg, "", irBasicBlock);
            std::vector<llvm::Value *> indices{
              llvm::ConstantInt::get(llvm::Type::getInt32Ty(stubFn->getContext()), 0),
              sp
            };
            auto *gep = llvm::GetElementPtrInst::CreateInBounds(RAM, indices, "", irBasicBlock);
            auto *bc = new llvm::BitCastInst(gep, spArg->getType(), "", irBasicBlock);
            auto *ret = new llvm::LoadInst(spArg->getType()->getPointerElementType(), bc, "", irBasicBlock);
            // Generate the call to retain the pointers
            std::vector<llvm::Value *> Arguments{PC, ret};
            auto *DHUPC = DuplicateFunction(this->HelperKeepPC, "KeepReturnAddress_" + std::to_string(HelperCount++));
            DHUPC->addFnAttr(llvm::Attribute::NoDuplicate);
            DHUPC->addFnAttr(llvm::Attribute::NoInline);
            DHUPC->addFnAttr(llvm::Attribute::NoMerge);
            auto *Res = llvm::CallInst::Create(DHUPC, Arguments, "", irBasicBlock);
            llvm::ReturnInst::Create(Context, Res, irBasicBlock);
          }
          // Exit block, a return in the partial CFG
          else {
            // (void)new llvm::UnreachableInst(Context, irBasicBlock);
            llvm::ReturnInst::Create(Context, PC, irBasicBlock);
          }
        } else {
          // This shouldn't be happening
          llvm::report_fatal_error("GetControlFlowGraph: unhandled basic block with 0 children!");
        }
      } break;
      case 1: {
        // Failure destination
        if (children[0] == 0) {
          // Exit block, a return in the partial CFG
          llvm::ReturnInst::Create(Context, PC, irBasicBlock);
        } else {
          // Fetch the destination basic block
          auto *BB0 = IRBasicBlocks.at(children[0]);
          // Is this the target basic block?
          if ((Target == address) && (basicBlock.BlockTy != whitepeacock::BlockType::Unconditional)) {
            // Possibly turning to conditional?
            auto *maybeDeadBlock = llvm::BasicBlock::Create(Context, "BB_" + to_string_hex(address) + ".maybe.dead", stubFn);
            // Possibly unreachable block, consider it as an exit
            llvm::ReturnInst::Create(
                Context, PC /*llvm::ConstantInt::get(stubFn->getFunctionType()->getReturnType(), 0xDEADEAD)*/,
                maybeDeadBlock);
            // Jump to both the basic blocks
            auto *BB1 = maybeDeadBlock;
            // Craft a label constant
            auto *LBL = llvm::ConstantInt::get(PC->getType(), children[0]);
            // Craft a comparison
            auto *Cmp = llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_EQ, PC, LBL,
                                              "cmp", irBasicBlock);
            // Jump to the basic blocks
            llvm::BranchInst::Create(BB0, BB1, Cmp, irBasicBlock);
          } else {
            // Jump to the basic block
            llvm::BranchInst::Create(BB0, irBasicBlock);
          }
        }
      } break;
      case 2: {
        // Fetch the destination basic blocks
        auto *BB0 = IRBasicBlocks.at(children[0]);
        auto *BB1 = IRBasicBlocks.at(children[1]);
        // Craft a label constant
        auto *LBL = llvm::ConstantInt::get(PC->getType(), children[0]);
        // Craft a comparison
        auto *Cmp = llvm::CmpInst::Create(llvm::Instruction::ICmp, llvm::ICmpInst::ICMP_EQ, PC, LBL,
                                          "cmp", irBasicBlock);
        // Jump to the basic blocks
        llvm::BranchInst::Create(BB0, BB1, Cmp, irBasicBlock);
      } break;
      default: {
        // Craft the switch instruction
        auto *SW = llvm::SwitchInst::Create(PC, IRBasicBlocks.at(children[0]), children.size() - 1,
                                            irBasicBlock);
        // Add the missing cases
        for (size_t i = 1; i < children.size(); i++) {
          SW->addCase(
              llvm::ConstantInt::get(llvm::dyn_cast<llvm::IntegerType>(PC->getType()), children[i]),
              IRBasicBlocks.at(children[i]));
        }
      } break;
      }
      // Add the children to the worklist
      for (uint64_t child : basicBlock.Children)
        Worklist.push(child);
    }
  } while (!Worklist.empty());
  // 7. Connect the entry block
  llvm::BranchInst::Create(IRBasicBlocks[EntryPoint], entryBasicBlock);
  // 8. Print the control flow graph
  if (Debug)
    stubFn->dump();
  // 9. Inline the basic blocks body
  for (auto *Call : Calls) {
    llvm::InlineFunctionInfo IFI;
    InlineFunction(*Call, IFI);
  }
  // 10. Return the function
  return stubFn;
}

llvm::Function *Lifter::GetFinalFunction(uint64_t EntryPoint, llvm::Function *CFG) const {
  std::stringstream ss;
  ss << "F_0x";
  ss << std::hex << EntryPoint;
  auto *fn = DuplicateFunction(this->HelperFunction, ss.str());
  auto &entryBlock = fn->getEntryBlock();
  for (auto &instr : entryBlock) {
    if (auto *call = llvm::dyn_cast<llvm::CallInst>(&instr)) {
      if (call->getCalledFunction() == this->HelperStub) {
        call->setCalledFunction(CFG);
        break;
      }
    }
  }
  return fn;
}

llvm::Function *Lifter::SliceProgramCounter(llvm::Function *basicBlockFn) {
  auto *fn = DuplicateFunction(this->HelperSlicePC, "SlicedCFG");
  auto &entryBlock = fn->getEntryBlock();
  for (auto &instr : entryBlock) {
    if (auto *call = llvm::dyn_cast<llvm::CallInst>(&instr)) {
      if (call->getCalledFunction() == this->HelperStub) {
        call->setCalledFunction(basicBlockFn);
        break;
      }
    }
  }
  return fn;
}
