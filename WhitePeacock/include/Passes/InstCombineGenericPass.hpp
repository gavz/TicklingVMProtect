#pragma once

#include <llvm/IR/Function.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PatternMatch.h>
#include <llvm/Pass.h>

#include <vector>
#include <set>

#include <Lifter/Lifter.hpp>

llvm::FunctionPass *getInstCombineGenericPassPass(OptimizationGuide &OG);