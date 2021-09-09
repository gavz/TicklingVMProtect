#include "Optimizer/Optimizer.hpp"

#include "llvm-c/Transforms/PassManagerBuilder.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Analysis/BasicAliasAnalysis.h"
#include "llvm/Analysis/CFLAndersAliasAnalysis.h"
#include "llvm/Analysis/CFLSteensAliasAnalysis.h"
#include "llvm/Analysis/ScalarEvolutionAliasAnalysis.h"
#include "llvm/Analysis/GlobalsModRef.h"
#include "llvm/Analysis/InlineCost.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Analysis/ScopedNoAliasAA.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TypeBasedAliasAnalysis.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Transforms/AggressiveInstCombine/AggressiveInstCombine.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/ForceFunctionAttrs.h"
#include "llvm/Transforms/IPO/FunctionAttrs.h"
#include "llvm/Transforms/IPO/InferFunctionAttrs.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Instrumentation.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/InstSimplifyPass.h"
#include "llvm/Transforms/Scalar/SimpleLoopUnswitch.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Vectorize.h"

#include "llvm/Transforms/IPO/AlwaysInliner.h"

#include <Passes/ConstantConcretizationPass.hpp>
#include <Passes/InstCombineGenericPass.hpp>
#include <Passes/PartialOverlapDSEPass.hpp>
#include <Passes/KnownIndexSelectPass.hpp>
#include <Passes/MemoryCoalescingPass.hpp>
#include <Passes/SynthesizeFlagsPass.hpp>
#include <Passes/PointersHoistPass.hpp>
#include <Passes/SegmentsAAPass.hpp>

#include <iostream>

using namespace llvm::legacy;
using namespace llvm;
using namespace std;

bool inlineRemillFunctions(Function *F) {
  // Find all the functions to be inlined
  set<CallInst *> FunctionsToInline;
  for (inst_iterator II = inst_begin(F), E = inst_end(F); II != E;) {
    auto I = II;
    II++;
    if (CallInst *CI = dyn_cast<CallInst>(&*I)) {
      // Check if the called instruction has a Attribute::AlwaysInline
      auto *CF = CI->getCalledFunction();
      if (CF && CF->hasFnAttribute(Attribute::AlwaysInline) && !CF->isDeclaration())
        FunctionsToInline.insert(CI);
    }
  }

  // Now inline all the functions
  for (auto *CI : FunctionsToInline) {
    InlineFunctionInfo IFI;
    InlineFunction(*CI, IFI);
  }
  return !FunctionsToInline.empty();
}

// Custom optimization pipeline

void addInitialAliasAnalysisPasses(legacy::PassManagerBase &PM, OptimizationGuide &Guide) {
  PM.add(createCFLSteensAAWrapperPass());
  PM.add(createCFLAndersAAWrapperPass());
  PM.add(createTypeBasedAAWrapperPass());
  PM.add(createScopedNoAliasAAWrapperPass());
  // Custom alias analysis pass
  if (Guide.RunSegmentsAliasAnalysisPass) {
    PM.add(createSegmentsAAWrapperPass());
    PM.add(new SegmentsExternalAAWrapperPass());
  }
}

void populateFunctionPassManager(legacy::FunctionPassManager &FPM, OptimizationGuide &Guide) {
  addInitialAliasAnalysisPasses(FPM, Guide);
  FPM.add(createLowerExpectIntrinsicPass());
  FPM.add(createCFGSimplificationPass());
  FPM.add(createSROAPass());
  FPM.add(createEarlyCSEPass());
}

void initializePipeline(llvm::legacy::FunctionPassManager &FPM, llvm::Module *M, OptimizationGuide &Guide) {
  // Configure the pass manager builder
  llvm::PassManagerBuilder PMB;
  PMB.OptLevel = 3;
  PMB.SizeLevel = 2;
  PMB.DisableUnrollLoops = true; //!Guide.RunLoopPasses;
  PMB.RerollLoops = false;
  PMB.SLPVectorize = false;
  PMB.LoopVectorize = false;
  PMB.Inliner = createFunctionInliningPass();
  // Initialize the early passes
  populateFunctionPassManager(FPM, Guide);
}

void optimizeFunction(llvm::Function *F, OptimizationGuide &Guide) {
  // Fetch the module
  auto *M = F->getParent();
  // Create the pass manager
  llvm::legacy::FunctionPassManager FPM(M);
  // Initialize the base pipeline
  initializePipeline(FPM, M, Guide);
  // Add the function passes
  FPM.add(createInstructionCombiningPass());
  FPM.add(createCFGSimplificationPass());
  FPM.add(createEarlyCSEPass(true));
  if (Guide.RunInstCombineGenericPass)
    FPM.add(getInstCombineGenericPassPass(Guide));
  if (Guide.RunKnownIndexSelectPass)
    FPM.add(getKnownIndexSelectPassPass(Guide));
  if (Guide.RunPointersHoistingPass)
    FPM.add(getPointersHoistPass(Guide));
  FPM.add(createGVNHoistPass());
  FPM.add(createGVNSinkPass());
  FPM.add(createCFGSimplificationPass());
  FPM.add(createJumpThreadingPass());
  FPM.add(createCorrelatedValuePropagationPass());
  FPM.add(createCFGSimplificationPass());
  FPM.add(createAggressiveInstCombinerPass());
  FPM.add(createInstructionCombiningPass());
  FPM.add(createReassociatePass());
  if (Guide.RunLoopPasses) {
    FPM.add(createLoopInstSimplifyPass());
    FPM.add(createLoopSimplifyCFGPass());
    FPM.add(createLICMPass(100, 250));
    FPM.add(createLoopRotatePass(0));
    FPM.add(createLICMPass(100, 250));
    FPM.add(createSimpleLoopUnswitchLegacyPass());
    FPM.add(createCFGSimplificationPass());
    FPM.add(createInstructionCombiningPass());
    FPM.add(createLoopFlattenPass());
    FPM.add(createLoopSimplifyCFGPass());
    FPM.add(createLoopIdiomPass());
    FPM.add(createIndVarSimplifyPass());
    FPM.add(createLoopDeletionPass());
    FPM.add(createLoopInterchangePass());
    FPM.add(createSimpleLoopUnrollPass(3, false, false));
  }
  FPM.add(createMergedLoadStoreMotionPass());
  FPM.add(createGVNPass(false));
  FPM.add(createBitTrackingDCEPass());
  FPM.add(createAggressiveDCEPass());
  FPM.add(createDeadStoreEliminationPass());
  FPM.add(createCFGSimplificationPass());
  FPM.add(createInstructionCombiningPass());
  if (Guide.RunMemoryCoalescingPass)
    FPM.add(getMemoryCoalescingPass(Guide));
  if (Guide.RunDeleteDeadStoresPass)
    FPM.add(getPartialOverlapDSEPass(Guide));
  FPM.add(createDeadStoreEliminationPass());
  if (Guide.RunConstantConcretizationPass)
    FPM.add(getConstantConcretizationPassPass(Guide));
  FPM.add(createDeadStoreEliminationPass()); // added
  FPM.add(createCFGSimplificationPass());    // added
  FPM.add(createInstructionCombiningPass()); // added
  FPM.add(createCFGSimplificationPass());    // added
  FPM.add(createDeadStoreEliminationPass()); // added
  if (Guide.RunFlagsSynthesisPass)
    FPM.add(getSynthesizeFlagsPass(Guide));

  // Apply the optimizations
  size_t MinInsCount = F->getInstructionCount();
  FPM.doInitialization();
  size_t Count = 0;
  do {
    Guide.HasChanged = false;
    FPM.run(*F);
    // Check if the function changed
    size_t InsCount = F->getInstructionCount();
    if (InsCount < MinInsCount) {
      MinInsCount = InsCount;
      Guide.HasChanged |= true;
    }
    // Bail out after 5 times
    if (Count++ == 5) {
      llvm::errs() << "[OPTIMIZER] bailing out on limit!\n";
      break;
    }
  } while (Guide.HasChanged);
  FPM.doFinalization();
}

void optimizeLLVM(llvm::Function *F) {
  llvm::legacy::FunctionPassManager func_manager(F->getParent());
  llvm::legacy::PassManager module_manager;
  llvm::PassManagerBuilder builder;
  builder.OptLevel = 3;
  builder.SizeLevel = 2;
  builder.DisableUnrollLoops = false;
  builder.MergeFunctions = false;
  builder.RerollLoops = false;
  builder.VerifyInput = false;
  builder.VerifyOutput = false;
  builder.Inliner = llvm::createFunctionInliningPass(250);

  builder.populateFunctionPassManager(func_manager);
  builder.populateModulePassManager(module_manager);

  func_manager.doInitialization();
  func_manager.run(*F);
  func_manager.doFinalization();

  module_manager.run(*F->getParent());
}