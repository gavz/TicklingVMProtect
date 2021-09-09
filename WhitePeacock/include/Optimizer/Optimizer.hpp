#pragma once

#include <Lifter/Lifter.hpp>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>

void optimizeLLVM(llvm::Function *F);
void optimizeFunction(llvm::Function *F, OptimizationGuide &Guide);
bool inlineRemillFunctions(llvm::Function *F);