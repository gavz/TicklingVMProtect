#include <Utilities/Utility.hpp>

// Clone function `source_func` into `dest_func`, using `value_map` to map over
// values. This will strip out debug info during the clone. This will strip out
// debug info during the clone.
//
// Note: this will try to clone globals referenced from the module of
//       `source_func` into the module of `dest_func`.
void CloneFunctionInto(llvm::Function *source_func, llvm::Function *dest_func,
                       ValueMap &value_map) {

  auto func_name = source_func->getName().str();
  auto source_mod = source_func->getParent();
  auto dest_mod = dest_func->getParent();
  auto &source_context = source_mod->getContext();
  auto &dest_context = dest_func->getContext();
  auto reg_md_id = source_context.getMDKindID("remill_register");

  // Make sure that when we're cloning `__remill_basic_block`, we don't
  // throw away register names and such.
  dest_func->getContext().setDiscardValueNames(false);
  dest_func->setAttributes(source_func->getAttributes());
  dest_func->setLinkage(source_func->getLinkage());
  dest_func->setVisibility(source_func->getVisibility());
  dest_func->setCallingConv(source_func->getCallingConv());
  dest_func->setIsMaterializable(source_func->isMaterializable());

  // Clone the basic blocks and their instructions.
  std::unordered_map<llvm::BasicBlock *, llvm::BasicBlock *> block_map;
  for (auto &old_block : *source_func) {
    auto new_block =
        llvm::BasicBlock::Create(dest_func->getContext(), old_block.getName(), dest_func);
    value_map[&old_block] = new_block;
    block_map[&old_block] = new_block;

    auto &new_insts = new_block->getInstList();
    for (auto &old_inst : old_block) {
      if (llvm::isa<llvm::DbgInfoIntrinsic>(old_inst))
        continue;

      auto new_inst = old_inst.clone();
      new_insts.push_back(new_inst);
      value_map[&old_inst] = new_inst;
    }
  }

  llvm::SmallVector<std::pair<unsigned, llvm::MDNode *>, 4> mds;

  // Fixup the references in the cloned instructions so that they point into
  // the cloned function, or point to declared globals in the module containing
  // `dest_func`.
  for (auto &old_block : *source_func) {
    for (auto &old_inst : old_block) {
      if (llvm::isa<llvm::DbgInfoIntrinsic>(old_inst))
        continue;

      auto new_inst = llvm::dyn_cast<llvm::Instruction>(value_map[&old_inst]);

      // Clear out all metadata from the new instruction.

      old_inst.getAllMetadata(mds);
      for (auto md_info : mds) {
        if (md_info.first != reg_md_id || &source_context != &dest_context)
          new_inst->setMetadata(md_info.first, nullptr);
      }

      new_inst->setDebugLoc(llvm::DebugLoc());
      new_inst->setName(old_inst.getName());

      for (auto &new_op : new_inst->operands()) {
        auto old_op_val = new_op.get();

        if (llvm::isa<llvm::Constant>(old_op_val) && !llvm::isa<llvm::GlobalValue>(old_op_val))
          continue; // Don't clone constants.

        // Already cloned the value, replace the old with the new.
        auto new_op_val_it = value_map.find(old_op_val);
        if (value_map.end() != new_op_val_it) {
          new_op.set(new_op_val_it->second);
          continue;
        }

        // At this point we could have an InlineAsm value
        if (auto *InAsm = llvm::dyn_cast<llvm::InlineAsm>(old_op_val))
          continue;

        // At this point, all we should have is a global.
        auto global_val = llvm::dyn_cast<llvm::GlobalValue>(old_op_val);
        if (!global_val) {
          llvm::errs() << "Cannot clone value from function " << func_name
                       << "because it isn't a global value.\n";
        }

        // If it's a global and we're in the same module, then use it.
        if (global_val && dest_mod == source_mod) {
          value_map[global_val] = global_val;
          new_op.set(global_val);
          continue;
        }

        // Declare the global in the new module.
        llvm::GlobalValue *new_global_val = nullptr;

        if (auto global_val_func = llvm::dyn_cast<llvm::Function>(global_val)) {
          auto new_func = dest_mod->getOrInsertFunction(
              global_val->getName(),
              llvm::dyn_cast<llvm::FunctionType>(global_val->getValueType()));

          new_global_val = llvm::dyn_cast<llvm::GlobalValue>(new_func.getCallee());

          if (auto as_func = llvm::dyn_cast<llvm::Function>(new_global_val)) {
            as_func->setAttributes(global_val_func->getAttributes());
          }

        } else if (llvm::isa<llvm::GlobalVariable>(global_val)) {
          new_global_val = llvm::dyn_cast<llvm::GlobalValue>(
              dest_mod->getOrInsertGlobal(global_val->getName(), global_val->getValueType()));

        } else {
          llvm::errs() << "Cannot clone value into new module for function " << func_name << "\n";
        }

        auto old_name = global_val->getName().str();
        auto new_name = new_global_val->getName().str();

        // Mark the global as extern, so that it can link back to the old
        // module.
        new_global_val->setLinkage(llvm::GlobalValue::ExternalLinkage);
        new_global_val->setVisibility(llvm::GlobalValue::DefaultVisibility);

        value_map[global_val] = new_global_val;
        new_op.set(new_global_val);
      }

      // Remap PHI node predecessor blocks.
      if (auto phi = llvm::dyn_cast<llvm::PHINode>(new_inst)) {
        for (auto i = 0UL; i < phi->getNumIncomingValues(); ++i) {
          phi->setIncomingBlock(i, block_map[phi->getIncomingBlock(i)]);
        }
      }
    }
  }
}

// Clone function `source_func` into `dest_func`. This will strip out debug
// info during the clone.
void CloneFunctionInto(llvm::Function *source_func, llvm::Function *dest_func) {
  auto new_args = dest_func->arg_begin();
  ValueMap value_map;
  for (llvm::Argument &old_arg : source_func->args()) {
    new_args->setName(old_arg.getName());
    value_map[&old_arg] = &*new_args;
    ++new_args;
  }

  CloneFunctionInto(source_func, dest_func, value_map);
}

// Duplicate a function in the same module

llvm::Function *DuplicateFunction(llvm::Function *F) {
  // 0. Generate the new function name
  const auto &DuplicateName = F->getName().str() + "_dup";
  // 1. Get the original function type
  auto *FT = F->getFunctionType();
  // 2. Create a new empty function
  auto *NF =
      llvm::Function::Create(FT, llvm::GlobalValue::InternalLinkage, DuplicateName, F->getParent());
  // 3. Clone the function body
  CloneFunctionInto(F, NF);
  // 4. Return the cloned function
  return NF;
}

// Duplicate a basic block in the same function

llvm::BasicBlock *
DuplicateBasicBlock(llvm::BasicBlock *BB,
                    std::unordered_map<llvm::Value *, llvm::Value *> &ValMapping) {
  // Fetch the function
  auto *F = BB->getParent();
  // Create a new basic block
  auto *NewBB = llvm::BasicBlock::Create(F->getContext(), BB->getName() + "_dup", F);
  // Copy the instructions
  auto &NewInstructions = NewBB->getInstList();
  for (auto &OldI : *BB) {
    auto *NewI = OldI.clone();
    NewInstructions.push_back(NewI);
    ValMapping[&OldI] = NewI;
  }
  // Return the basic block
  return NewBB;
}

// Get the topologically ordered basic blocks from a function

std::vector<llvm::BasicBlock *> getOrderedBasicBlocks(llvm::Function *F) {
  // Do you a favor and read this pearl:
  // https://eli.thegreenplace.net/2013/09/16/analyzing-function-cfgs-with-llvm/
  std::vector<llvm::BasicBlock *> BasicBlocks;
  // Order the basic blocks (even if the CFG contains loops)
  for (llvm::scc_iterator<llvm::Function *> I = scc_begin(F), IE = scc_end(F); I != IE; ++I) {
    // Obtain the vector of BBs in this SCC and save it
    const std::vector<llvm::BasicBlock *> &SCCBBs = *I;
    for (std::vector<llvm::BasicBlock *>::const_iterator BBI = SCCBBs.begin(), BBIE = SCCBBs.end();
         BBI != BBIE; ++BBI) {
      BasicBlocks.insert(BasicBlocks.begin(), *BBI);
    }
  }
  // Return the ordered basic blocks
  return BasicBlocks;
}

// Get the topologically ordered instructions from a function

std::vector<llvm::Instruction *> getOrderedInstructions(llvm::Function *F) {
  // 1. We want to return instructions in a DFS fashion
  std::vector<llvm::Instruction *> Instructions;
  // 2. We get the basic blocks in a DFS fashion
  for (auto *BB : getOrderedBasicBlocks(F))
    for (auto &I : *BB)
      Instructions.push_back(&I);
  // 3. Return the ordered instructions
  return Instructions;
}

// Topologically sort a CFG

void topologicallySortCFG(llvm::Function *F) {
  // Get the sorted list of basic blocks in the function
  auto SortedBasicBlocks = getOrderedBasicBlocks(F);
  // Get the unsorted list of basic blocks in the function
  auto &BasicBlocks = F->getBasicBlockList();
  llvm::SmallVector<llvm::BasicBlock *, 16> UnsortedBasicBlocks;
  for (auto I = BasicBlocks.begin(); I != BasicBlocks.end();) {
    auto *B = BasicBlocks.remove(I);
    UnsortedBasicBlocks.push_back(B);
  }
  // Update the basic blocks
  BasicBlocks.insert(BasicBlocks.begin(), SortedBasicBlocks.begin(), SortedBasicBlocks.end());
}

// Clone an instructions chain

llvm::Instruction *cloneInstructionsChain(llvm::Instruction *I, llvm::Instruction *IP) {
  // Use a cloning structure
  typedef struct ClonedInstruction {
    llvm::Instruction *OrigI = nullptr;
    llvm::Instruction *CopyI = nullptr;
  } ClonedInstruction;
  // Keep track of the cloned instructions
  std::unordered_map<llvm::Value *, llvm::Value *> Mapping;
  std::vector<ClonedInstruction> Cloned;
  // Use a worklist
  std::stack<llvm::Instruction *> Worklist{{I}};
  std::set<llvm::Value *> Known;
  while (!Worklist.empty()) {
    // Fetch the value
    auto *I = Worklist.top();
    Worklist.pop();
    // Skip if known
    if (Known.find(I) != Known.end())
      continue;
    Known.insert(I);
    // Clone the instruction
    auto *CI = I->clone();
    // Save the instruction
    Cloned.push_back({I, CI});
    // Update the mapping
    Mapping[I] = CI;
    // Add the operands to the worklist
    for (size_t i = 0; i < I->getNumOperands(); i++) {
      if (auto *O = llvm::dyn_cast<llvm::Instruction>(I->getOperand(i))) {
        Worklist.push(O);
      }
    }
  }
  // Order the instructions by dominance
  llvm::DominatorTree DT(*I->getFunction());
  std::sort(Cloned.begin(), Cloned.end(),
            [&DT](const ClonedInstruction &A, const ClonedInstruction &B) {
              return DT.dominates(A.OrigI, B.OrigI);
            });
  // Insert the cloned instructions
  for (auto &ClonedI : Cloned) {
    // Insert the instruction
    ClonedI.CopyI->insertAfter(IP);
    // Update the operands
    for (size_t i = 0; i < ClonedI.CopyI->getNumOperands(); i++) {
      auto IT = Mapping.find(ClonedI.CopyI->getOperand(i));
      if (IT != Mapping.end()) {
        ClonedI.CopyI->setOperand(i, IT->second);
      }
    }
    // Update the insertion point
    IP = ClonedI.CopyI;
  }
  // Return the new chain
  return llvm::dyn_cast<llvm::Instruction>(Mapping[I]);
}