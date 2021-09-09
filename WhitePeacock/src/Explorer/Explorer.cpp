#include <souper/Extractor/Candidates.h>
#include <souper/Extractor/ExprBuilder.h>
#include <souper/Extractor/Solver.h>

#include <llvm/Support/CommandLine.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/CFG.h>

#include <Synthesis/SolverCache.hpp>
#include <Optimizer/Optimizer.hpp>
#include <Explorer/Explorer.hpp>
#include <Utilities/Utility.hpp>

#include <iostream>
#include <fstream>

// Arguments

llvm::cl::list<std::string> ConstantsMap("constants-map",
  llvm::cl::desc("Specify a map address:value like: 0x1000:0x1234,0x1004:0x5678"),
  llvm::cl::value_desc("constants"),
  llvm::cl::CommaSeparated);

llvm::cl::opt<bool> SynthesizeFlags("synthesize-flags",
  llvm::cl::desc("Synthesize the flags with Souper"),
  llvm::cl::value_desc("SynthesizeFlags"),
  llvm::cl::init(true),
  llvm::cl::Optional);

llvm::cl::opt<bool> ApplyUndefine("apply-undefine",
  llvm::cl::desc("Convert the __undef into an undef value"),
  llvm::cl::value_desc("ApplyUndefine"),
  llvm::cl::init(true),
  llvm::cl::Optional);

llvm::cl::opt<bool> LoopOptimisations("loop-optimisations",
  llvm::cl::desc("Run the loop optimisations, especially a high cost loop unrolling"),
  llvm::cl::value_desc("LoopOptimisations"),
  llvm::cl::init(true),
  llvm::cl::Optional);

llvm::cl::opt<bool> InstCombineGeneric("instcombine-generic",
  llvm::cl::desc("Pattern match the missing LLVM patterns"),
  llvm::cl::value_desc("InstCombineGeneric"),
  llvm::cl::init(false),
  llvm::cl::Optional);

llvm::cl::opt<bool> ConcretizeLoadedConstants("concretize-constants",
  llvm::cl::desc("Concretize the loaded constants"),
  llvm::cl::value_desc("ConcretizeLoadedConstants"),
  llvm::cl::init(true),
  llvm::cl::Optional);

llvm::cl::opt<bool> SegmentsAliasAnalysis("segments-alias-analysis",
  llvm::cl::desc("NoAlias for pointers on different segments"),
  llvm::cl::value_desc("SegmentsAliasAnalysis"),
  llvm::cl::init(true),
  llvm::cl::Optional);

llvm::cl::opt<bool> MemoryCoalescing("memory-coalescing",
  llvm::cl::desc("Enable the memory coalescing pass"),
  llvm::cl::value_desc("MemoryCoalescing"),
  llvm::cl::init(true),
  llvm::cl::Optional);

llvm::cl::opt<bool> PointersHoisting("pointer-hoisting",
  llvm::cl::desc("Hoists the pointer calculation to the entry block"),
  llvm::cl::value_desc("PointersHoisting"),
  llvm::cl::init(true),
  llvm::cl::Optional);

llvm::cl::opt<bool> PartialOverlapDSE("partial-overlap-dse",
  llvm::cl::desc("Kills the dead stores not removed by the plain DSE"),
  llvm::cl::value_desc("PartialOverlapDSE"),
  llvm::cl::init(false),
  llvm::cl::Optional);

llvm::cl::opt<bool> KnownIndexSelect("known-index-select",
  llvm::cl::desc("Simplifies the conditional jumps used by VMProtect 3.0.9"),
  llvm::cl::value_desc("KnownIndexSelect"),
  llvm::cl::init(true),
  llvm::cl::Optional);

llvm::cl::opt<bool> EnableDebug("enable-debug",
  llvm::cl::desc("Enables the debug information"),
  llvm::cl::value_desc("EnableDebug"),
  llvm::cl::init(false),
  llvm::cl::Optional);

llvm::cl::opt<bool> PrintSlicedPC("print-sliced-pc",
  llvm::cl::desc("Enables the debug printing of the sliced program counter"),
  llvm::cl::value_desc("PrintSlicedPC"),
  llvm::cl::init(false),
  llvm::cl::Optional);

// Methods

Explorer::Explorer(uint64_t address, Lifter &mLifter, BinaryParserStream &stream) :
  mAddress(address), mLifter(mLifter), mStream(stream) {
  // Push the first virtual stub address
  mVirtualStubsWorklist.push(address);
}

void Explorer::identifyVirtualBlockDestinations(llvm::Function *F, std::set<uint64_t> &Destinations, const std::unordered_map<uint64_t, uint64_t> &ConstantsMap, size_t SolutionsLimit, size_t SolverTimeout, bool PrintSMTQuery) {
  // Collect the constant solutions
  std::vector<ProvedConstant> RecoveredConstants;
  std::vector<uint64_t> MappedConstants;
  // Find the HelperKeepPC call
  auto *HKPC = F->getParent()->getFunction("HelperKeepPC");
  llvm::CallInst *Ret = nullptr;
  for (auto *U : HKPC->users()) {
    if (auto *CI = llvm::dyn_cast<llvm::CallInst>(U)) {
      if (CI->getFunction() == F) {
        Ret = CI;
        break;
      }
    }
  }
  if (Ret) {
    // Use Souper
    souper::InstContext IC;
    souper::ReplacementContext RC;
    souper::ExprBuilderContext EBC;
    souper::ExprBuilderOptions EBO;
    souper::FunctionCandidateSet FCS;
    // Fetch the returned value
    auto *RetValue = Ret->getOperand(0);
    // Handle the constant case
    if (auto *C = llvm::dyn_cast<llvm::ConstantInt>(RetValue)) {
      uint64_t Value = C->getLimitedValue();
      // Ignoring the invalid exit used by VMProtect to panic
      if (Value)
        Destinations.insert(Value);
      return;
    }
    // Handle the API case
    if (auto *L = llvm::dyn_cast<llvm::LoadInst>(RetValue)) {
      // Check if it's a read from constant memory
      if (auto *CE =
              llvm::dyn_cast<llvm::ConstantExpr>(L->getPointerOperand()->stripPointerCasts())) {
        // Fetch the RAM pointer
        auto *RAM = F->getParent()->getGlobalVariable("RAM");
        // Check if it's a read from the memory
        if (CE->getOperand(0) == RAM) {
          if (auto *C = llvm::dyn_cast<llvm::ConstantInt>(CE->getOperand(2))) {
            // We spotted the API address, we could identify the return address too
            return;
          }
        }
      }
    }
    // Handle the return case
    if (auto *L = llvm::dyn_cast<llvm::LoadInst>(RetValue)) {
      // Fetch the STACK base pointer and register
      auto *STACK = F->getParent()->getGlobalVariable("STACK");
      auto *RAM = F->getParent()->getGlobalVariable("RAM");
      auto *SP = F->getArg(7);
      // Check if it's a read from the stack
      if (auto *GEP = llvm::dyn_cast<llvm::GetElementPtrInst>(
              L->getPointerOperand()->stripPointerCasts())) {
        if (GEP->getPointerOperand() == STACK || GEP->getPointerOperand() == RAM) {
          if (GEP->getOperand(2) == SP) {
            return;
          } else if (auto *BinOp = llvm::dyn_cast<llvm::BinaryOperator>(GEP->getOperand(2))) {
            if (BinOp->getOpcode() == llvm::Instruction::Add) {
              if (BinOp->getOperand(0) == SP) {
                return;
              }
            }
          }
        }
      }
    }
    // Handle the jump to register case
    if (auto *A = llvm::dyn_cast<llvm::Argument>(RetValue))
      return;
    // Handle the jump to memory
    if (auto *L = llvm::dyn_cast<llvm::LoadInst>(RetValue)) {
      // Fetch the RAM base pointer and register
      auto *RAM = F->getParent()->getGlobalVariable("RAM");
      // Check if it's a read from the memory
      if (auto *GEP = llvm::dyn_cast<llvm::GetElementPtrInst>(
              L->getPointerOperand()->stripPointerCasts())) {
        if (GEP->getPointerOperand() == RAM) {
          if (auto *BinOp = llvm::dyn_cast<llvm::BinaryOperator>(GEP->getOperand(2)))
            if (llvm::isa<llvm::Argument>(BinOp->getOperand(0)))
              return;
        }
      }
    }
    // At last try to use Souper to solve the destinations
    EBO.CandidateFilterInstruction = RetValue;
    // Extract the candidates
    FCS = souper::ExtractCandidates(F, IC, EBC, EBO);
    // Identify the candidate
    souper::CandidateReplacement *CR = nullptr;
    for (auto &B : FCS.Blocks) {
      for (auto &R : B->Replacements) {
        if ((R.Origin != EBO.CandidateFilterInstruction) || (R.Mapping.LHS->Width != 64))
          continue;
        CR = &R;
        break;
      }
    }
    if (!CR)
      llvm::report_fatal_error("identifyChildren (2): no available candidates!");
    // Get the solutions
    souper::Inst *Solution = nullptr;
    // Create a new variable
    souper::Inst *ConstantVar = IC.createVar(CR->Mapping.LHS->Width, "ConstantVar");
    // Map the variable to the left-hand side
    souper::InstMapping IM(CR->Mapping.LHS, ConstantVar);
    // Search the valid solutions
    do {
      std::vector<souper::Inst *> ModelInstructions;
      std::vector<llvm::APInt> ModelValues;
      // Populate the preconditions
      std::vector<souper::Inst *> Preconditions;
      souper::Inst *Precondition = nullptr;
      // Build the exclusions
      for (const auto &RecoveredConstant : RecoveredConstants) {
        souper::Inst *NotConstant =
            IC.getInst(souper::Inst::Ne, 64, {CR->Mapping.LHS, RecoveredConstant.I});
        Preconditions.push_back(NotConstant);
      }
      // Add the preconditions
      if (!Preconditions.empty())
        Precondition = IC.getInst(souper::Inst::And, 1, Preconditions);
      // Build the query
      souper::BlockPCs BPCs;
      std::vector<souper::InstMapping> PCs;
      // TODO: verify why the BPCs and PCs may lead to no results sometimes
      // const auto &Query = souper::BuildQuery(IC, CR->BPCs, CR->PCs, IM, &ModelInstructions, Precondition, true);
      const auto &Query = souper::BuildQuery(IC, BPCs, PCs, IM, &ModelInstructions, Precondition, true);
      // Debug print the SMT query if requested
      if (PrintSMTQuery) {
        llvm::outs() << "[+] SMT Query:\n";
        llvm::outs() << Query << "\n";
      }
      // Solve the query
      bool IsSat = false;
      auto Error = SolverCache::Get().isSatisfiableCached(Query, IsSat, ModelInstructions.size(), &ModelValues, SolverTimeout);
      // Check if we hit an error
      if (Error) {
        llvm::outs() << "Hit an error while solving the branches: " << Error.message() << "\n";
        std::cin.get();
      }
      // Check if we satisfied the query
      if (!IsSat)
        break;
      // Parse the solution
      Solution = nullptr;
      for (int i = 0; i < ModelValues.size(); i++) {
        if (ModelInstructions[i]->Name == "ConstantVar")
          Solution = IC.getConst(ModelValues[i]);
      }
      // Check if we got a solution
      if (!Solution)
        break;
      // Store the solution
      auto Proved = ProvedConstant(Solution, Solution->Val.getLimitedValue());
      RecoveredConstants.push_back(Proved);
      // Stop if we reached the limit
      if (RecoveredConstants.size() > SolutionsLimit)
        break;
    } while (Solution);
  }
  // Save the detected children
  for (const auto &RecoveredConstant : RecoveredConstants)
    Destinations.insert(RecoveredConstant.V);
}

void Explorer::identifyVirtualStubDestinations(VirtualStub &vmStub) {
  // 1. Collect useful pointers
  auto *F = vmStub.CFG;
  auto *RAM = F->getParent()->getGlobalVariable("RAM");
  // 2. We expect to find the memory pointer
  if (!RAM) {
    std::stringstream ss;
    ss << __func__;
    ss << ": missing RAM global variable!";
    llvm::report_fatal_error(ss.str());
  }
  // 3. Identify the exit blocks (containing a call to KeepReturnAddress)
  std::map<llvm::CallInst *, llvm::BasicBlock *> exitBlocks;
  std::set<llvm::Function *> functions;
  for (auto &BB : *F) {
    for (auto &I : BB) {
      if (auto *C = llvm::dyn_cast<llvm::CallInst>(&I)) {
        auto *CF = C->getCalledFunction();
        if (CF && CF->getName().startswith("KeepReturnAddress_")) {
          functions.insert(CF);
          exitBlocks[C] = &BB;
        }
      }
    }
  }
  // 4. Parse the exit blocks
  for (const auto &EB : exitBlocks) {
    const auto *R0 = EB.first->getOperand(0);
    const auto *R1 = EB.first->getOperand(1);
    const auto *Block = EB.second;
    // Keep track if we detected an exit case
    bool detected = false;
    // 4.1 Constant case
    if (const auto *C = llvm::dyn_cast<llvm::ConstantInt>(R0)) {
      UnOps opcodes;
      std::set<uint64_t> nextPCs;
      uint64_t nextPC = 0;
      uint64_t PC = C->getLimitedValue();
      if (PC == 0) {
        std::cout << "[VMProtectPanic] 0x" << std::hex << PC << std::endl;
        detected = true;
      } else if (mStream.isPossibleVirtualStub(PC)) {
        // Found a real virtual stub address
        vmStub.Children.insert(C);
        vmStub.Destinations.insert(PC);
        detected = true;
      } else if (mStream.isUnsupportedInstructionExit(PC, nextPC, opcodes)) {
        std::cout << "[UnsupportedInstruction] 0x" << std::hex << PC << std::endl;
        vmStub.Children.insert(llvm::ConstantInt::get(C->getType(), nextPC));
        vmStub.Destinations.insert(nextPC);
        vmStub.Unsupported.insert(C);
        vmStub.UnsupportedMap[PC] = nextPC;
        vmStub.UnsupportedOpcodes[PC] = opcodes;
        detected = true;
      } else if (const auto *R = llvm::dyn_cast<llvm::ConstantInt>(R1)) {
        uint64_t RET = R->getLimitedValue();
        if (mStream.isPossibleVirtualStub(RET)) {
          std::cout << "[FunctionCall] 0x" << std::hex << PC << " (" << mStream.getImportName(PC) << ")" << std::endl;
          vmStub.Calls.insert(C);
          vmStub.Children.insert(R);
          vmStub.Destinations.insert(RET);
          detected = true;
        }
      } else if (const auto *S = llvm::dyn_cast<llvm::SelectInst>(R1)) {
        if (const auto *C2 = llvm::dyn_cast<llvm::ConstantInt>(S->getOperand(1))) {
          if (const auto *C3 = llvm::dyn_cast<llvm::ConstantInt>(S->getOperand(2))) {
            uint64_t PC2 = C2->getLimitedValue();
            uint64_t PC3 = C3->getLimitedValue();
            if (mStream.isPossibleVirtualStub(PC2) && mStream.isPossibleVirtualStub(PC3)) {
              std::cout << "[FunctionCall] 0x" << std::hex << PC << " (" << mStream.getImportName(PC) << ")" << std::endl;
              vmStub.Calls.insert(C);
              vmStub.Children.insert(C2);
              vmStub.Children.insert(C3);
              vmStub.Destinations.insert(PC2);
              vmStub.Destinations.insert(PC3);
              detected = true;
            }
          }
        }
      } else {
        std::cout << "[FunctionJump] 0x" << std::hex << PC << std::endl;
        detected = true;
      }
    }
    // 4.2 Return or indirect call case
    else if (const auto *L = llvm::dyn_cast<llvm::LoadInst>(R0)) {
      const auto *P = L->getPointerOperand()->stripPointerCasts();
      if (const auto *CE = llvm::dyn_cast<llvm::ConstantExpr>(P)) {
        if (CE->getOpcode() == llvm::Instruction::GetElementPtr) {
          if (auto *GEP = llvm::dyn_cast<llvm::GetElementPtrInst>(CE->getAsInstruction())) {
            if (GEP->getPointerOperand() == RAM && GEP->getNumIndices() == 2)
              if (const auto *C = llvm::dyn_cast<llvm::ConstantInt>(GEP->getOperand(2))) {
                if (const auto *R = llvm::dyn_cast<llvm::ConstantInt>(R1)) {
                  uint64_t PC = C->getLimitedValue();
                  uint64_t RET = R->getLimitedValue();
                  if (mStream.isPossibleVirtualStub(RET)) {
                    std::cout << "[FunctionCall] 0x" << std::hex << PC << " (" << mStream.getImportName(PC) << ")" << std::endl;
                    vmStub.Calls.insert(C);
                    vmStub.Children.insert(R);
                    vmStub.Destinations.insert(RET);
                    detected = true;
                  }
                } else if (const auto *S = llvm::dyn_cast<llvm::SelectInst>(R1)) {
                  if (const auto *C2 = llvm::dyn_cast<llvm::ConstantInt>(S->getOperand(1))) {
                    if (const auto *C3 = llvm::dyn_cast<llvm::ConstantInt>(S->getOperand(2))) {
                      uint64_t PC = C->getLimitedValue();
                      uint64_t PC2 = C2->getLimitedValue();
                      uint64_t PC3 = C3->getLimitedValue();
                      if (mStream.isPossibleVirtualStub(PC2) && mStream.isPossibleVirtualStub(PC3)) {
                        std::cout << "[FunctionCall] 0x" << std::hex << PC << " (" << mStream.getImportName(PC) << ")" << std::endl;
                        vmStub.Calls.insert(C);
                        vmStub.Children.insert(C2);
                        vmStub.Children.insert(C3);
                        vmStub.Destinations.insert(PC2);
                        vmStub.Destinations.insert(PC3);
                        detected = true;
                      }
                    }
                  }
                } else {
                  uint64_t PC = C->getLimitedValue();
                  std::cout << "[IndirectJump] 0x" << std::hex << PC << std::endl;
                  vmStub.Calls.insert(C);
                  detected = true;
                }
              }
            GEP->deleteValue();
          }
        }
      } else if (const auto *GEP = llvm::dyn_cast<llvm::GetElementPtrInst>(P)) {
        if (GEP->getPointerOperand() == RAM && GEP->getNumIndices() == 2) {
          if (const auto *BinOp = llvm::dyn_cast<llvm::BinaryOperator>(GEP->getOperand(2))) {
            if (const auto *Load = llvm::dyn_cast<llvm::LoadInst>(BinOp->getOperand(0))) {
              if (const auto *Const = llvm::dyn_cast<llvm::ConstantInt>(BinOp->getOperand(1))) {
                if (const auto *Arg = llvm::dyn_cast<llvm::Argument>(Load->getPointerOperand())) {
                  if (Arg->getName() == "rsp" || Arg->getName() == "esp") {
                    if (!Const->isNegative()) {
                      std::cout << "[FunctionReturn] ret" << std::endl;
                      detected = true;
                    } else if (Const->isNegative()) {
                      // This is likely an unpropagated exit value (aliasing issue)
                      std::stack<const llvm::BasicBlock *> worklist;
                      std::set<const llvm::ConstantInt *> addresses;
                      std::set<const llvm::BasicBlock *> known;
                      worklist.push(EB.second);
                      do {
                        // Fetch the basic block
                        const auto *BB = worklist.top();
                        worklist.pop();
                        // Skip if known
                        if (known.count(BB))
                          continue;
                        known.insert(BB);
                        // Search for the expected store
                        bool found = false;
                        for (const auto &I : *BB) {
                          if (const auto *S = llvm::dyn_cast<llvm::StoreInst>(&I)) {
                            if (S->getPointerOperand()->stripPointerCasts() == GEP) {
                              if (const auto *C = llvm::dyn_cast<llvm::ConstantInt>(S->getOperand(0))) {
                                if (mStream.isPossibleVirtualStub(C->getLimitedValue())) {
                                  addresses.insert(C);
                                  found = true;
                                }
                              } else if (const auto *P = llvm::dyn_cast<llvm::PHINode>(S->getOperand(0))) {
                                for (unsigned i = 0; i < P->getNumIncomingValues(); i++) {
                                  if (const auto *C = llvm::dyn_cast<llvm::ConstantInt>(P->getIncomingValue(i))) {
                                    addresses.insert(C);
                                    found = true;
                                  } else {
                                    found = false;
                                    break;
                                  }
                                }
                              }
                            }
                          }
                        }
                        // Fetch the predecessors
                        if (!found) {
                          for (auto *PBB : llvm::predecessors(BB))
                            worklist.push(PBB);
                        }
                      } while (!worklist.empty());
                      // Determine if we found valid address(es)
                      if (!addresses.empty()) {
                        for (const auto *C : addresses) {
                          vmStub.Children.insert(C);
                          vmStub.Destinations.insert(C->getLimitedValue());
                        }
                        detected = true;
                      }
                      // Debug pause if we found more valid addresses
                      if (addresses.size() > 1) {
                        llvm::outs() << "Found more unpropagated valid addresses:\n";
                        for (const auto *C : addresses)
                          llvm::outs() << "> " << llvm::format_hex(C->getLimitedValue(), 0, true) << "\n";
                        std::cin.get();
                      }
                    }
                  }
                }
              }
            }
          } else if (const auto *L1 = llvm::dyn_cast<llvm::LoadInst>(GEP->getOperand(2))) {
            if (const auto *Arg = llvm::dyn_cast<llvm::Argument>(L1->getPointerOperand())) {
              if (Arg->getName() == "rsp" || Arg->getName() == "esp") {
                std::cout << "[FunctionReturn] ret" << std::endl;
                detected = true;
              }
            }
          }
        }
      }
    }
    // 4.3 Unsupported edge case, non-propagated value or indirect call
    if (!detected) {
      llvm::outs() << "[UnmatchedJump] ";
      R0->dump();
    }
  }
  // 5. Delete the 'KeepReturnAddress' functions
  for (auto *CF : functions) {
    for (auto *U : CF->users()) {
      if (auto *C = llvm::dyn_cast<llvm::CallInst>(U)) {
        C->replaceAllUsesWith(C->getOperand(0));
        C->eraseFromParent();
      }
    }
    CF->eraseFromParent();
  }
}

void Explorer::generateVirtualStubEpilogue(VirtualStub &vmStub) {
  // Fetch the function
  auto *F = vmStub.CFG;
  // Fetch the context
  auto &Ctx = F->getContext();
  // Fetch the stack, RAM and __undef pointers
  auto *UND = F->getParent()->getGlobalVariable("__undef");
  auto *RAM = F->getParent()->getGlobalVariable("RAM");
  llvm::Argument *SP = nullptr;
  for (auto &A : F->args()) {
    if (A.getName() == "rsp" || A.getName() == "esp")
      SP = &A;
  }
  if (!RAM || !UND || !SP) {
    std::stringstream ss;
    ss << __func__;
    ss << ": missing RAM, UND or stack pointer!";
    llvm::report_fatal_error(ss.str());
  }
  // Lambda function to generate a call epilogue block
  auto generateCallEpilogue = [&](llvm::ReturnInst *ret, llvm::Value *addr) {
    // Determine the architecture address type
    auto *AddrTy = ret->getOperand(0)->getType();
    // Determine the inline asm function type
    auto *AsmTy = llvm::FunctionType::get(llvm::Type::getVoidTy(Ctx), { AddrTy }, false);
    // Determine the architecture tag
    std::string AddrTag;
    size_t AddrSz = 0;
    if (auto *AddrIntTy = llvm::dyn_cast<llvm::IntegerType>(AddrTy)) {
      switch (AddrIntTy->getBitWidth()) {
        case 64: {
          AddrTag = "qword";
          AddrSz = 8;
        } break;
        case 32: {
          AddrTag = "dword";
          AddrSz = 4;
        } break;
        default: {
          std::stringstream ss;
          ss << __func__;
          ss << ": unsupported bit size!";
          llvm::report_fatal_error(ss.str());
        } break;
      }
    }
    // %und0 = load i64, i64* @__undef
    auto *und0 = new llvm::LoadInst(AddrTy, UND, "", ret);
    // %rsp0 = load i64, i64* %rsp
    auto *xsp0 = new llvm::LoadInst(AddrTy, SP, "", ret);
    // %ptr0 = getelementptr inbounds [0 x i8], [0 x i8]* @RAM, i64 0, i64 %rsp0
    std::vector<llvm::Value *> Indices{ llvm::ConstantInt::get(AddrTy, 0), xsp0 };
    auto *ptr0 = llvm::GetElementPtrInst::CreateInBounds(RAM->getType()->getPointerElementType(), RAM, Indices, "", ret);
    // %ptr1 = bitcast i8* %ptr0 to i64*
    auto *ptr1 = new llvm::BitCastInst(ptr0, AddrTy->getPointerTo(), "", ret);
    // %ret0 = load i64, i64* %ptr1
    auto *ret0 = new llvm::LoadInst(AddrTy, ptr1, "", ret);
    // store i64 %und0, i64* %ptr1
    auto *str0 = new llvm::StoreInst(und0, ptr1, ret);
    // %rsp1 = add i64 %rsp0, 8
    auto *xsp1 = llvm::BinaryOperator::CreateAdd(xsp0, llvm::ConstantInt::get(AddrTy, AddrSz), "", ret);
    // store i64 %rsp1, i64* %rsp
    auto *str1 = new llvm::StoreInst(xsp1, SP, ret);
    // tail call void asm sideeffect "call qword $0", "r,~{memory}"(i64 %11) #1
    auto *asm0 = llvm::InlineAsm::get(AsmTy, "call " + AddrTag + " $0", "r,~{memory}", true, false, llvm::InlineAsm::AsmDialect::AD_ATT);
    auto *call = llvm::CallInst::Create(asm0, { addr }, "", ret);
    // ret i64 %ret0
    ret->setOperand(0, ret0);
  };
  // Lambda function to generate an unsupported instruction(s) epilogue block
  auto generateUnsupportedInstructionsEpilogue = [&](llvm::ReturnInst *ret, llvm::Value *addr) {
    if (auto *C = llvm::dyn_cast<llvm::ConstantInt>(addr)) {
      // Fetch the unsupported instruction(s) information
      uint64_t UnAddr = C->getLimitedValue();
      uint64_t ReAddr = vmStub.UnsupportedMap[UnAddr];
      const auto &Opcodes = vmStub.UnsupportedOpcodes[UnAddr];
      // TODO: lift the unsupported instructions
      for (const auto &Opcode : Opcodes) {
        for (const uint8_t Byte : Opcode)
          std::cout << std::hex << (int)Byte << std::endl;
      }
      // Update the return address
      ret->setOperand(0, llvm::ConstantInt::get(addr->getType(), ReAddr));
    }
  };
  // Identify the return instructions
  std::vector<llvm::ReturnInst *> returns;
  for (auto &BB : *F) {
    if (auto *R = llvm::dyn_cast<llvm::ReturnInst>(BB.getTerminator()))
      returns.push_back(R);
  }
  // Handle the exit block(s)
  for (auto *RetI : returns) {
    auto *RetV = RetI->getOperand(0);
    if (auto *P = llvm::dyn_cast<llvm::PHINode>(RetV)) {
      // Multiple exit blocks
      bool allCalls = true;
      for (size_t i = 0; i < P->getNumIncomingValues() && allCalls; i++) {
        if (!vmStub.Calls.count(P->getIncomingValue(i)))
          allCalls = false;
      }
      if (allCalls) {
        generateCallEpilogue(returns[0], P);
      } else {
        std::stringstream ss;
        ss << __func__;
        ss << ": the PHI contains a non-call address (stub or unsupported instruction), the exit block must be splitted!";
        llvm::report_fatal_error(ss.str());
      }
    } else {
      // Single exit block
      if (vmStub.Calls.count(RetV)) {
        generateCallEpilogue(returns[0], RetV);
      } else if (vmStub.Unsupported.count(RetV)) {
        generateUnsupportedInstructionsEpilogue(returns[0], RetV);
      }
    }
  }
}

void Explorer::convertGetElementPtrToIntToPtr(llvm::Function *F) {
  auto *RAM = F->getParent()->getGlobalVariable("RAM");
  for (auto &BB : *F) {
    for (auto &I : BB) {
      if (auto *L = llvm::dyn_cast<llvm::LoadInst>(&I)) {
        auto *Ptr = L->getPointerOperand()->stripPointerCasts();
        if (auto *GEP = llvm::dyn_cast<llvm::GetElementPtrInst>(Ptr)) {
          if (GEP->getPointerOperand() == RAM && GEP->getNumIndices() == 2) {
            auto *ITP = new llvm::IntToPtrInst(GEP->getOperand(2), L->getPointerOperand()->getType(), "", &I);
            L->setOperand(0, ITP);
          }
        } else if (auto *CE = llvm::dyn_cast<llvm::ConstantExpr>(Ptr)) {
          if (CE->getOperand(0) == RAM) {
            auto *ITP = new llvm::IntToPtrInst(CE->getOperand(2), L->getPointerOperand()->getType(), "", &I);
            L->setOperand(0, ITP);
          }
        }
      } else if (auto *S = llvm::dyn_cast<llvm::StoreInst>(&I)) {
        auto *Ptr = S->getPointerOperand()->stripPointerCasts();
        if (auto *GEP = llvm::dyn_cast<llvm::GetElementPtrInst>(Ptr)) {
          if (GEP->getPointerOperand() == RAM && GEP->getNumIndices() == 2) {
            auto *ITP = new llvm::IntToPtrInst(GEP->getOperand(2), S->getPointerOperand()->getType(), "", &I);
            S->setOperand(1, ITP);
          }
        } else if (auto *CE = llvm::dyn_cast<llvm::ConstantExpr>(Ptr)) {
          if (CE->getOperand(0) == RAM) {
            auto *ITP = new llvm::IntToPtrInst(CE->getOperand(2), S->getPointerOperand()->getType(), "", &I);
            S->setOperand(1, ITP);
          }
        }
      }
    }
  }
}

std::set<uint64_t> Explorer::updateEdges(std::map<uint64_t, std::set<uint64_t>> &Edges,
  const std::unordered_map<uint64_t, whitepeacock::BasicBlock> &VirtualBlocks,
  std::set<uint64_t> &VirtualBlocksExplored,
  whitepeacock::BasicBlock &Block) {
  // Debug dump the edges
  if (EnableDebug) {
    llvm::outs() << "================================\n";
    llvm::outs() << "Block: " << llvm::format_hex(Block.EntryPoint, 0, true) << "\n";
    llvm::outs() << "Children: ";
    for (const auto Child : Block.Children)
      llvm::outs() << llvm::format_hex(Child, 0, true) << " ";
    llvm::outs() << "\n";
    llvm::outs() << "Edges (before): " << Edges.size() << "\n";
    for (const auto &E : Edges) {
      llvm::outs() << llvm::format_hex(E.first, 0, true) << " => ";
      for (const auto D : E.second)
        llvm::outs() << llvm::format_hex(D, 0, true) << " ";
      llvm::outs() << "\n";
    }
    llvm::outs() << "Explored: " << VirtualBlocksExplored.size() << "\n";
    for (const auto E : VirtualBlocksExplored)
      llvm::outs() << "> " << llvm::format_hex(E, 0, true) << "\n";
  }
  // Loop the children of the basic block
  std::set<uint64_t> toReprove;
  for (uint64_t Child : Block.Children) {
    // Did we meet this child before?
    if (Edges.count(Child)) {
      // Did we explore this connection before?
      bool Unexplored = false;
      auto B = Edges.find(Block.EntryPoint);
      if (B != Edges.end()) {
        // Did we know this edge before?
        if (!B->second.count(Child))
          Unexplored = true;
      } else {
        Unexplored = true;
      }
      // Was the edge unexplored?
      if (Unexplored) {
        // Mark the new chain as unexplored
        std::stack<uint64_t> Chain{{Child}};
        std::set<uint64_t> Known;
        do {
          // Fetch the address
          uint64_t Address = Chain.top();
          Chain.pop();
          // Skip if already handled
          if (Known.count(Address))
            continue;
          Known.insert(Address);
          // Do we need to reprove the block?
          bool toBeReproved = true;
          // If unconditional/exit, we don't need to
          auto VB = VirtualBlocks.find(Address);
          if (VB != VirtualBlocks.end()) {
            if (VB->second.BlockTy == whitepeacock::BlockType::Unconditional)
              toBeReproved = false;
          }
          // Mark the new block as in need to be reproved
          if (toBeReproved) {
            if (EnableDebug)
              llvm::outs() << "Dead: " << llvm::format_hex(Address, 0, true) << "\n";
            VirtualBlocksExplored.erase(Address);
            toReprove.insert(Address);
          }
          // Add its children to the chain
          auto I = Edges.find(Address);
          if (I != Edges.end()) {
            for (uint64_t Child : I->second)
              Chain.push(Child);
          }
        } while (!Chain.empty());
      }
    }
  }
  // Handle the special self-loop case, only first hit
  if (Block.Children.count(Block.EntryPoint)) {
    if (!Edges.count(Block.EntryPoint))
      VirtualBlocksExplored.erase(Block.EntryPoint);
  }
  // Create a new entry in the edges if unknown
  if (!Edges.count(Block.EntryPoint))
    Edges[Block.EntryPoint] = {};
  // Update the blocks connections
  for (uint64_t Child : Block.Children)
    Edges[Block.EntryPoint].insert(Child);
  // Debug dump the edges
  if (EnableDebug) {
    llvm::outs() << "Edges (after): " << Edges.size() << "\n";
    for (const auto &E : Edges) {
      llvm::outs() << llvm::format_hex(E.first, 0, true) << " => ";
      for (const auto D : E.second)
        llvm::outs() << llvm::format_hex(D, 0, true) << " ";
      llvm::outs() << "\n";
    }
    llvm::outs() << "Explored: " << VirtualBlocksExplored.size() << "\n";
    for (const auto E : VirtualBlocksExplored)
      llvm::outs() << "> " << llvm::format_hex(E, 0, true) << "\n";
    llvm::outs() << "================================\n";
  }
  // Return the blocks to be proved again
  return toReprove;
}

whitepeacock::BlockType Explorer::isUnconditionalBasicBlock(const llvm::Function *F, uint64_t &destination) const {
  auto *EntryBB = &F->getEntryBlock();
  if (auto *Ret = llvm::dyn_cast<llvm::ReturnInst>(EntryBB->getTerminator())) {
    if (auto *BinOp = llvm::dyn_cast<llvm::BinaryOperator>(Ret->getOperand(0))) {
      if (BinOp->getOpcode() == llvm::Instruction::Add) {
        if (auto *Load = llvm::dyn_cast<llvm::LoadInst>(BinOp->getOperand(0))) {
          if (auto *Const = llvm::dyn_cast<llvm::ConstantInt>(BinOp->getOperand(1))) {
            if (auto *Gep = llvm::dyn_cast<llvm::GetElementPtrInst>(Load->getPointerOperand())) {
              if (auto *Arg = llvm::dyn_cast<llvm::Argument>(Gep->getPointerOperand())) {
                destination = Const->getLimitedValue();
                if (!mStream.isPossibleVirtualStub(destination))
                  if (Arg->getName() == "slots")
                    return whitepeacock::BlockType::MaybeUnconditional;
              }
            } else if (auto *Arg = llvm::dyn_cast<llvm::Argument>(Load->getPointerOperand())) {
              destination = Const->getLimitedValue();
              if (!mStream.isPossibleVirtualStub(destination)) 
                if (Arg->getName() == "slots")
                  return whitepeacock::BlockType::MaybeUnconditional;
            }
          }
        } else if (auto *Arg = llvm::dyn_cast<llvm::Argument>(BinOp->getOperand(0))) {
          if (auto *Const = llvm::dyn_cast<llvm::ConstantInt>(BinOp->getOperand(1)))
            if (Arg->getName() == "REL_ADDR") {
              destination = Const->getLimitedValue();
              if (!mStream.isPossibleVirtualStub(destination))
                return whitepeacock::BlockType::Unconditional;
            }
        }
      }
    } else if (auto *Const = llvm::dyn_cast<llvm::ConstantInt>(Ret->getOperand(0))) {
      destination = Const->getLimitedValue();
      if (!mStream.isPossibleVirtualStub(destination))
        return whitepeacock::BlockType::Unconditional;
    }
  }
  return whitepeacock::BlockType::Conditional;
}

bool Explorer::run() {
  // Lambda function to move the blocks to be re-explored in the worklist
  auto refillWorklist = [](std::set<uint64_t> &reprove, std::queue<uint64_t> &worklist) {
    if (worklist.empty()) {
      for (const auto address : reprove)
        worklist.push(address);
      reprove.clear();
    }
  };
  // Parse the constants map
  std::unordered_map<uint64_t, uint64_t> Constants;
  for (const auto &Mapping : ConstantsMap) {
    std::vector<std::string> Values;
    std::stringstream SS(Mapping);
    std::string Item;
    while (std::getline(SS, Item, ':'))
      Values.push_back(Item);
    if (Values.size() == 2) {
      uint64_t Address = std::strtoull(Values[0].data(), nullptr, 16);
      uint64_t Value = std::strtoull(Values[1].data(), nullptr, 16);
      Constants[Address] = Value;
    }
  }
  // Configure the optimization pipeline
  OptimizationGuide guide;
  guide.Constants = Constants;
  guide.RunKnownIndexSelectPass = KnownIndexSelect;
  guide.RunMemoryCoalescingPass = MemoryCoalescing;
  guide.RunPointersHoistingPass = PointersHoisting;
  // Populate the binary sections
  const auto *binary = mStream.getObjectFilePointer();
  for (const auto &section : binary->sections()) {
    uint64_t begin = section.getAddress();
    uint64_t end = begin + section.getSize();
    guide.Sections[begin] = end;
  }
  // Proceed with the exploration
  do {
    // Fetch the next virtual stub address
    uint64_t virtualStubAddress = mVirtualStubsWorklist.front();
    mVirtualStubsWorklist.pop();
    // Ignore an explored address
    if (mVirtualStubsExplored.count(virtualStubAddress))
      continue;
    mVirtualStubsExplored.insert(virtualStubAddress);
    // Debug print
    std::cout << "Handling virtual stub: 0x" << std::hex << virtualStubAddress << std::endl;
    // Create and save a new virtual stub
    mVirtualStubs[virtualStubAddress] = { .EntryPoint = virtualStubAddress };
    // Keep track of the explored virtual blocks
    std::unordered_map<uint64_t, whitepeacock::BasicBlock> VirtualBlocks;
    // Keep track of the proved edges between virtual blocks
    std::map<uint64_t, std::set<uint64_t>> Edges;
    // Keep track of the conditional nodes that got 1 solution
    std::set<uint64_t> UnconditionalConditionals;
    // Explore the virtual stub
    std::set<uint64_t> VirtualBlocksReprove;
    std::set<uint64_t> VirtualBlocksExplored;
    std::queue<uint64_t> VirtualBlocksWorklist;
    VirtualBlocksWorklist.push(virtualStubAddress);
    do {
      // Fetch the next address
      uint64_t basicBlockAddress = VirtualBlocksWorklist.front();
      VirtualBlocksWorklist.pop();
      // Ignore an explored address
      if (VirtualBlocksExplored.count(basicBlockAddress)) {
        // Refill the worklist if there are blocks to re-explore
        refillWorklist(VirtualBlocksReprove, VirtualBlocksWorklist);
        mStream.killSnapshot(basicBlockAddress);
        continue;
      }
      // Debug print
      std::cout << "[WORKLIST]: " << std::dec << VirtualBlocksWorklist.size() << std::endl;
      std::cout << "[EXPLORED]: " << std::dec << VirtualBlocksExplored.size() << std::endl;
      std::cout << "[VIRBLOCK]: 0x" << std::hex << basicBlockAddress << std::endl;
      VirtualBlocksExplored.insert(basicBlockAddress);
      // Fetching the basic block
      whitepeacock::BasicBlock basicBlock;
      if (!VirtualBlocks.count(basicBlockAddress)) {
        // Fetch the basic block
        if (!mStream.getBasicBlock(basicBlockAddress, basicBlock)) {
          std::stringstream ss;
          ss << __func__;
          ss << ": failing on a missing basic block ";
          ss << "(0x" << std::hex << basicBlockAddress << ")";
          ss << std::endl;
          llvm::report_fatal_error(ss.str());
        }
        // Lift the basic block
        auto *liftedVirtualBlock = mLifter.LiftBasicBlock(basicBlock);
        // Optimize the basic block
        guide.RunSegmentsAliasAnalysisPass = SegmentsAliasAnalysis;
        mLifter.OptimizeFunction(liftedVirtualBlock, guide, false, false, true);
        guide.RunSegmentsAliasAnalysisPass = false;
        // Save the lifted code
        basicBlock.LiftedCode = liftedVirtualBlock;
        // Determine if it's unconditional
        uint64_t destination = 0;
        auto blockType = isUnconditionalBasicBlock(liftedVirtualBlock, destination);
        if (basicBlock.BlockTy == whitepeacock::BlockType::Unconditional) {
          llvm::outs() << "[EXIT]\n";
        } else if (blockType == whitepeacock::BlockType::Unconditional) {
          llvm::outs() << "[UNCONDITIONAL]\n";
          basicBlock.BlockTy = blockType;
          basicBlock.Children.insert(destination);
        } else {
          llvm::outs() << "[UNKNOWN]\n";
          basicBlock.BlockTy = blockType;
        }
        // Save the basic block into the map
        VirtualBlocks[basicBlockAddress] = basicBlock;
      }
      // Skip the solving if it's an unconditional or exit block
      if (VirtualBlocks[basicBlockAddress].BlockTy != whitepeacock::BlockType::Unconditional) {
        // Create the partial control flow function
        auto *partialCFG = mLifter.GetControlFlowGraph(virtualStubAddress, basicBlockAddress, VirtualBlocks, true);
        // Slice the program counter
        auto *slicedPC = mLifter.SliceProgramCounter(partialCFG);
        // Optimize first without custom passes
        OptimizationGuide empty;
        guide.RunLoopPasses = LoopOptimisations;
        mLifter.OptimizeFunction(slicedPC, empty, ApplyUndefine, false, false);
        guide.RunLoopPasses = false;
        // Optimize using the concrete constants (useful for the jump tables)
        guide.RunLoopPasses = LoopOptimisations;
        guide.RunSegmentsAliasAnalysisPass = SegmentsAliasAnalysis;
        guide.RunConstantConcretizationPass = ConcretizeLoadedConstants;
        mLifter.OptimizeFunction(slicedPC, guide, false, false, true);
        guide.RunConstantConcretizationPass = false;
        guide.RunSegmentsAliasAnalysisPass = false;
        guide.RunLoopPasses = false;
        // Print the sliced PC
        if (PrintSlicedPC)
          slicedPC->dump();
        // Identify the destination basic blocks
        identifyVirtualBlockDestinations(slicedPC, VirtualBlocks[basicBlockAddress].Children, guide.Constants);
        // Sanity check, there's something super fishy or broken if we got no solution
        if (VirtualBlocks[basicBlockAddress].Children.size() == 0) {
          slicedPC->dump();
          llvm::outs() << "We got 0 solutions, something is super fishy or broken!\n";
          std::cin.get();
        }
        // Sanity check, there's something fishy if we got 1 solution
        else if (VirtualBlocks[basicBlockAddress].Children.size() == 1) {
          if (VirtualBlocks[basicBlockAddress].BlockTy == whitepeacock::BlockType::Conditional)
            UnconditionalConditionals.insert(basicBlockAddress);
        // All good, we got 2 solutions or more
        } else if (UnconditionalConditionals.count(basicBlockAddress)) {
          // Remove the block from from the unconditional conditionals
          UnconditionalConditionals.erase(basicBlockAddress);
        }
        // Sanity check, there's something fishy if we got more than 2 solutions, maybe a switch?
        if (VirtualBlocks[basicBlockAddress].Children.size() > 2) {
          slicedPC->dump();
          std::cout << "Detected more than 2 children, maybe a switch case?" << std::endl;
          std::cin.get();
        }
        // Delete the partial control flow and sliced PC functions
        partialCFG->eraseFromParent();
        slicedPC->eraseFromParent();
      }
      // Resolve the virtual basic block addresses
      if (mStream.setBasicBlockAddresses(basicBlockAddress, VirtualBlocks[basicBlockAddress].Children)) {
        // Add the destinations to the worklist
        for (const uint64_t Destination : VirtualBlocks[basicBlockAddress].Children) {
          std::cout << "[VirtualBlock] 0x" << std::hex << Destination << std::endl;
          VirtualBlocksWorklist.push(Destination);
        }
      } else {
        // Reset the children of an exit block
        VirtualBlocks[basicBlockAddress].Children.clear();
      }
      // Update the proved edges
      for (auto Destination : updateEdges(Edges, VirtualBlocks, VirtualBlocksExplored, VirtualBlocks[basicBlockAddress])) {
        std::cout << "[ProveAgain] 0x" << std::hex << Destination << std::endl;
        VirtualBlocksReprove.insert(Destination);
      }
      // Refill the worklist if there are blocks to re-explore
      refillWorklist(VirtualBlocksReprove, VirtualBlocksWorklist);
    } while (!VirtualBlocksWorklist.empty());
    // Debug dump the connections
    llvm::outs() << "[BLOCKS] " << Edges.size() << "\n";
    for (const auto &E : Edges) {
      llvm::outs() << llvm::format_hex(E.first, 0, true) << " => ";
      for (const auto D : E.second)
        llvm::outs() << llvm::format_hex(D, 0, true) << " ";
      llvm::outs() << "\n";
    }
    // Debug dump the unconditional conditionals
    if (!UnconditionalConditionals.empty()) {
      llvm::outs() << "[!] Found possibly conditional blocks with only 1 output edge, maybe:\n";
      llvm::outs() << "[!] > the partial control flow forces the condition to be opaque;\n";
      llvm::outs() << "[!] > there is a bug in the logic for the edge discovery.\n";
      llvm::outs() << "[!] Either ways, you are invited to manually check them for correctness!\n";
      for (auto Block : UnconditionalConditionals)
        llvm::outs() << "[!] > " << llvm::format_hex(Block, 0, true) << "\n";
      std::cin.get();
    }
    // Craft the final control flow graph
    auto *CFG = mLifter.GetControlFlowGraph(virtualStubAddress, virtualStubAddress, VirtualBlocks, false);
    auto *FF = mLifter.GetFinalFunction(virtualStubAddress, CFG);
    // Optimize the function (standard optimisations)
    mLifter.OptimizeFunction(FF, guide, false, true, false);
    // Optimize the function (custom alias analysis)
    guide.RunSegmentsAliasAnalysisPass = SegmentsAliasAnalysis;
    mLifter.OptimizeFunction(FF, guide, false, true, false);
    guide.RunSegmentsAliasAnalysisPass = false;
    // Optimize the function (flags synthesis)
    guide.RunFlagsSynthesisPass = SynthesizeFlags;
    mLifter.OptimizeFunction(FF, guide, false, true, false);
    guide.RunFlagsSynthesisPass = false;
    // Optimize the function (standard optimisations)
    guide.RunSegmentsAliasAnalysisPass = SegmentsAliasAnalysis;
    mLifter.OptimizeFunction(FF, guide, false, true, false);
    guide.RunSegmentsAliasAnalysisPass = false;
    // Optimize the function (using MSSA + loops)
    guide.RunLoopPasses = LoopOptimisations;
    guide.RunDeleteDeadStoresPass = PartialOverlapDSE;
    guide.RunSegmentsAliasAnalysisPass = SegmentsAliasAnalysis;
    mLifter.OptimizeFunction(FF, guide, false, true, false);
    guide.RunSegmentsAliasAnalysisPass = false;
    guide.RunDeleteDeadStoresPass = false;
    guide.RunLoopPasses = false;
    // Optimize the function (removing the undefines)
    mLifter.OptimizeFunction(FF, guide, ApplyUndefine, true, true);
    // Topologically sort the function
    topologicallySortCFG(FF);
    // Save the LLVM function address
    mVirtualStubs[virtualStubAddress].CFG = FF;
    // Identify the destination(s) of the virtual stub
    identifyVirtualStubDestinations(mVirtualStubs[virtualStubAddress]);
    // Add the destinations to the virtual stubs queue
    for (const uint64_t Destination : mVirtualStubs[virtualStubAddress].Destinations) {
      std::cout << "[VirtualStub] 0x" << std::hex << Destination << std::endl;
      mVirtualStubsWorklist.push(Destination);
    }
    // Delete the control flow graph
    CFG->eraseFromParent();
    // Optimize again to remove the dead code
    mLifter.OptimizeFunction(FF, guide, false, true, true);
    // Generate the epilogue for the virtual stub
    // generateVirtualStubEpilogue(mVirtualStubs[virtualStubAddress]);
    // Convert the 'getelementptr' RAM accesses into 'inttoptr'
    convertGetElementPtrToIntToPtr(FF);
    // Optimize again to remove the dead code
    mLifter.OptimizeFunction(FF, guide, false, true, true);
    // Debug pause
    mVirtualStubs[virtualStubAddress].CFG->dump();
    std::cin.get();
  } while (!mVirtualStubsWorklist.empty());
  // TODO: build the virtual function CFG
  // TODO: optimize the virtual function CFG
  // TODO: save the virtual function CFG to an *.ll file
  return true;
}