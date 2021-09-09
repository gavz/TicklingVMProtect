#include <Passes/InstCombineGenericPass.hpp>

using namespace llvm;
using namespace PatternMatch;

// https://llvm.org/docs/WritingAnLLVMPass.html

namespace {

struct InstCombineGenericPass : FunctionPass {
  static char ID;

  OptimizationGuide *mOG = nullptr;

  InstCombineGenericPass() : FunctionPass(ID) {}
  InstCombineGenericPass(OptimizationGuide &OG) : FunctionPass(ID), mOG(&OG) {}

  bool runOnFunction(Function &F) override {
    bool HasChanged = false;
    // Fetch the module pointer
    auto *M = F.getParent();
    // Loop all the instructions in the function
    for (inst_iterator I = inst_begin(F), E = inst_end(F); I != E; I++) {
      // Match the following pattern missing from LLVM: https://reviews.llvm.org/D84664
      {
        // Simplify the ZF pattern
        // %27 = select i1 %26, i64 64, i64 0
        // %28 = lshr exact i64 %27, 6
        // --------------------------------------------------------------------------
        // %28 = select i1 %26, i64 4096, i64 0
        Value *SelectCond = nullptr;
        ConstantInt *ShiftRHS = nullptr;
        ConstantInt *SelectLHS = nullptr;
        ConstantInt *SelectRHS = nullptr;
        if (m_LShr(
                m_Select(m_Value(SelectCond), m_ConstantInt(SelectLHS), m_ConstantInt(SelectRHS)),
                m_ConstantInt(ShiftRHS))
                .match(&*I)) {
          // Fetch the constants
          uint64_t SelectLHSVal = SelectLHS->getLimitedValue();
          uint64_t SelectRHSVal = SelectRHS->getLimitedValue();
          uint64_t ShiftRHSVal = ShiftRHS->getLimitedValue();
          // Calculate the shifted values
          auto *NewSelectLHS =
              llvm::ConstantInt::get(SelectLHS->getType(), SelectLHSVal >> ShiftRHSVal);
          auto *NewSelectRHS =
              llvm::ConstantInt::get(SelectRHS->getType(), SelectRHSVal >> ShiftRHSVal);
          // Craft a new select instruction
          auto *NewSelect =
              llvm::SelectInst::Create(SelectCond, NewSelectLHS, NewSelectRHS, "", &*I);
          // Replace the old instruction
          I->replaceAllUsesWith(NewSelect);
          // Mark the function as modified
          HasChanged = true;
          // Go to the next instruction
          continue;
        }
      }
      // Match the following pattern missing from LLVM: https://alive2.llvm.org/ce/z/93m7Q5
      {
        // Simplify the SELECT pattern
        // %e0 = sext i1 %cc to i64
        // %e1 = or i64 %e0, -5369864816
        // %e2 = xor i64 %e1, -1
        // %e3 = and i64 %e0, 5369865045
        // %e4 = add nuw nsw i64 %e3, %e2
        // --------------------------------------------------------------------------
        // %e0 = select i1 %cc, i64 5369865045, i64 5369864815
        llvm::Value *Cond0;
        llvm::Value *Cond1;
        llvm::ConstantInt *Const0 = nullptr;
        llvm::ConstantInt *Const1 = nullptr;
        if (m_c_Add(m_And(m_SExt(m_Value(Cond0)), m_ConstantInt(Const0)),
                    m_Xor(m_Or(m_SExt(m_Value(Cond1)), m_ConstantInt(Const1)), m_SpecificInt(-1)))
                .match(&*I)) {
          if (Cond0 == Cond1) {
            // Fetch the normalized values
            uint64_t Value0 = Const0->getLimitedValue();
            uint64_t Value1 = ~(Const1->getLimitedValue());
            // Craft the new constants
            auto *NewConst0 = llvm::ConstantInt::get(Const0->getType(), Value0);
            auto *NewConst1 = llvm::ConstantInt::get(Const1->getType(), Value1);
            // Craft a new select instruction
            auto *NewSelect = llvm::SelectInst::Create(Cond0, NewConst0, NewConst1, "", &*I);
            // Replace the old instruction
            I->replaceAllUsesWith(NewSelect);
            // Mark the function as modified
            HasChanged = true;
            // Go to the next instruction
            continue;
          }
        }
      }
    }
    if (mOG)
      mOG->HasChanged |= HasChanged;
    // Notify about the changes
    return HasChanged;
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override { AU.setPreservesCFG(); }

}; // namespace

char InstCombineGenericPass::ID = 0;

static RegisterPass<InstCombineGenericPass> X("InstCombineGenericPass",
                                              "Handles some patterns missing from LLVM",
                                              false /* Only looks at CFG */,
                                              false /* Analysis Pass */);

} // end of anonymous namespace

FunctionPass *getInstCombineGenericPassPass(OptimizationGuide &OG) {
  return new InstCombineGenericPass(OG);
}
