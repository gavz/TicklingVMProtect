#pragma once

#include <stack>
#include <unordered_map>

#include <llvm/ADT/SmallVector.h>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Utils/ValueMapper.h>

#include <llvm/ADT/SCCIterator.h>
#include <llvm/Analysis/CFG.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/InstIterator.h>

using ValueMap = std::unordered_map<llvm::Value *, llvm::Value *>;

// Clone function `source_func` into `dest_func`, using `value_map` to map over
// values. This will strip out debug info during the clone. This will strip out
// debug info during the clone.
//
// Note: this will try to clone globals referenced from the module of
//       `source_func` into the module of `dest_func`.
void CloneFunctionInto(llvm::Function *source_func, llvm::Function *dest_func, llvm::ValueToValueMapTy &value_map);

// Clone function `source_func` into `dest_func`. This will strip out debug
// info during the clone.
//
// Note: this will try to clone globals referenced from the module of
//       `source_func` into the module of `dest_func`.
void CloneFunctionInto(llvm::Function *source_func, llvm::Function *dest_func);

// Duplicate the function

llvm::Function *DuplicateFunction(llvm::Function *F);

// Duplicate the basic block

llvm::BasicBlock *DuplicateBasicBlock(llvm::BasicBlock *BB,
                                      std::unordered_map<llvm::Value *, llvm::Value *> &ValMapping);

// Get the topologically ordered basic blocks from a function

std::vector<llvm::BasicBlock *> getOrderedBasicBlocks(llvm::Function *F);

// Get the topologically ordered instructions from a function

std::vector<llvm::Instruction *> getOrderedInstructions(llvm::Function *F);

// Topologically sort a CFG

void topologicallySortCFG(llvm::Function *F);

// Clone an instructions chain

llvm::Instruction *cloneInstructionsChain(llvm::Instruction *I, llvm::Instruction *IP);