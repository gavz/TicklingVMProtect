#pragma once

#include <unordered_map>

// LLVM include

#include <llvm/object/ObjectFile.h>
#include <llvm/ADT/SmallString.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Linker/Linker.h>
#include <llvm/Support/Path.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Transforms/Utils/Cloning.h>

// Utility include

#include <Utilities/Utility.hpp>

#include <Streams/BasicBlock.hpp>

// Configuration structure

typedef struct OptimizationGuide {
  std::unordered_map<uint64_t, uint64_t> Constants;
  std::unordered_map<uint64_t, uint64_t> Sections;
  bool RunMemoryCoalescingPass = false;
  bool RunDeleteDeadStoresPass = false;
  bool RunInstCombineGenericPass = false;
  bool RunKnownIndexSelectPass = false;
  bool RunSegmentsAliasAnalysisPass = false;
  bool RunConstantConcretizationPass = false;
  bool RunPointersHoistingPass = false;
  bool RunFlagsSynthesisPass = false;
  bool RunLoopPasses = false;
  bool HasChanged = false;
} OptimizationGuide;

// Lifter class definition

struct ArgumentHelper;

class Lifter {
private:
  // Current LLVM module
  std::shared_ptr<llvm::Module> Module;

  // Helpers interface
  llvm::Function *HelperSlicePC = nullptr;
  llvm::Function *HelperFunction = nullptr;
  llvm::Function *HelperStubEmpty = nullptr;
  llvm::Function *HelperKeepPC = nullptr;
  llvm::Function *HelperStub = nullptr;
  llvm::GlobalVariable *UndefVariable = nullptr;
  std::map<std::string, llvm::Function *> Semantics;

  // Load the helpers
  void LoadHelpersInterface();

  // Lift an instruction

  void LiftInstruction(const whitepeacock::Instruction &instruction, llvm::BasicBlock *insertAtEnd,
                       ArgumentHelper &argumentHelper);

  // Duplicate a function
  static llvm::Function *DuplicateFunction(llvm::Function *F, const std::string &FN);

  static llvm::Function *GetParentFunction(llvm::Instruction *inst);
  static const llvm::Function *GetParentFunction(const llvm::Instruction *inst);

public:
  explicit Lifter(std::shared_ptr<llvm::Module> HelpersModule);

  // Dump a known function

  void DumpFunction(const std::string &Name) const;

  // Optimize a function

  void OptimizeFunction(llvm::Function *F, OptimizationGuide &Guide, bool Undefine = false,
                        bool Clean = false, bool Strip = false) const;

  // Interface for CFGExplorer TODO
  // throws
  llvm::Function *GetFinalFunction(uint64_t EntryPoint, llvm::Function *CFG) const;
  llvm::Function *LiftBasicBlock(const whitepeacock::BasicBlock &basicBlock);
  llvm::Function *SliceProgramCounter(llvm::Function *basicBlockFn);
  llvm::Function *GetControlFlowGraph(uint64_t EntryPoint, uint64_t Target,
                                      const std::unordered_map<uint64_t, whitepeacock::BasicBlock> &BasicBlocks,
                                      bool IsPartialCFG = false, bool Debug = false) const;
};
