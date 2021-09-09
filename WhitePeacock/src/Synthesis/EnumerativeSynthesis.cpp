// Copyright 2018 The Souper Authors. All rights reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include <Synthesis/EnumerativeSynthesis.hpp>
#include <Synthesis/SolverCache.hpp>
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/CommandLine.h"

#include "souper/Infer/Pruning.h"
#include "souper/Infer/ConstantSynthesis.h"

#include <functional>
#include <queue>
#include <set>

static const unsigned MaxTries = 30;
static const unsigned MaxInputSpecializationTries = 2;

const unsigned MyDebugLevel = 0;

using namespace souper;
using namespace llvm;

static const std::vector<Inst::Kind> UnaryOperators = { Inst::CtPop };

static const std::vector<Inst::Kind> BinaryOperators = {
  Inst::Eq, Inst::Ne, Inst::Ult,
  Inst::Slt, Inst::Ule, Inst::Sle,
  Inst::SAddO, Inst::UAddO, Inst::SSubO,
  Inst::USubO, Inst::SMulO, Inst::UMulO
};

static const std::vector<Inst::Kind> TernaryOperators = {};

const unsigned MaxNumInstructions = 1;
const unsigned MaxV = 150;
const bool AliveFlagParser = false;
const bool LSBPruning = true;
const bool EnableDataflowPruning = true;
const bool SynthesisConstWithCegisLoop = true;
const bool DoubleCheckWithAlive = false;
const bool SkipSolver = false;
const bool IgnoreCost = false;
const unsigned MaxLHSCands = 50;
const bool OnlyInferI1 = false;
const bool OnlyInferIN = false;

// TODO
// tune the constants at the top of the file
// constant synthesis
//   try to first make small constants? -128-255?
// multiple instructions
//   make a fresh constant per new binary/ternary instruction
// call solver in batches -- need to work on batch size
//   tune batch size -- can be a lot bigger for simple RHSs
//   or look for feedback from solver, timeouts etc.
// make sure path conditions work as expected
// once an optimization works we can try adding UB qualifiers on the RHS
//   probably almost as good as synthesizing these directly
// prune a subtree once it becomes clear it isn't a cost win
// aggressively prune dumb instructions
//   which logical operations are equivalent at 1 bit? (include icmps in this)
// add constraints guarding against synthesizing dumb constants
//   add x, 0; sub x, 0; shift x, 0; mul x, 0; mul x, 1, mul x, 2
//   shift left by 1
// aggressively prune dumb instruction sequences
//   trunc of trunc, sext of sext, zext of zext
//   trunc to i1 vs. %w = and %v, 1; %x = icmp ne %w, 0
//   a bloom filter or something
//   use the solver to recognize non-minimal instructions / instruction
//   sequences?
// we want to avoid guessing code that's already there on the LHS
//   hashing?
// test against CEGIS with LHS components
// test the width matching stuff
// take outside uses into account -- only in the cost model?
// experiment with synthesizing at reduced bitwidth, then expanding the result
// aggressively avoid calling into the solver

namespace synthesis {

void addGuess(souper::Inst *RHS, unsigned TargetWidth, souper::InstContext &IC,
              int MaxCost, std::vector<souper::Inst *> &Guesses,
              int &TooExpensive) {
  if (TargetWidth > RHS->Width) {
    auto NSExt = IC.getInst(souper::Inst::SExt, TargetWidth, {RHS});
    auto NZExt = IC.getInst(souper::Inst::ZExt, TargetWidth, {RHS});
    addGuess(NSExt, TargetWidth, IC, MaxCost, Guesses, TooExpensive);
    addGuess(NZExt, TargetWidth, IC, MaxCost, Guesses, TooExpensive);
  } else if (TargetWidth < RHS->Width) {
    auto NTrunc = IC.getInst(souper::Inst::Trunc, TargetWidth, {RHS});
    addGuess(NTrunc, TargetWidth, IC, MaxCost, Guesses, TooExpensive);
  } else {
    if (IgnoreCost || souper::cost(RHS) < MaxCost)
      Guesses.push_back(RHS);
    else
      TooExpensive++;
  }
}

// Does a short-circuiting AND operation
PruneFunc MkPruneFunc(std::vector<PruneFunc> Funcs) {
  return [Funcs](souper::Inst *I, std::vector<souper::Inst *> &RI) {
    for (auto F : Funcs) {
      if (!F(I, RI)) {
        return false;
      }
    }
    return true;
  };
}

bool CountPrune(souper::Inst *I, std::vector<souper::Inst *> &ReservedInsts,
                std::set<souper::Inst *> Visited) {
  return !(souper::countHelper(I, Visited) > MaxNumInstructions);
}

// TODO(manasij/zhengyang) souper::cost needs a caching layer
template <typename Container>
void sortGuesses(Container &Guesses) {
  // One of the real advantages of enumerative synthesis vs
  // CEGIS is that we can synthesize in precisely increasing cost
  // order, and not try to somehow teach the solver how to do that
  std::stable_sort(Guesses.begin(), Guesses.end(),
                   [](souper::Inst *a, souper::Inst *b) -> bool {
                     return souper::cost(a) < souper::cost(b);
                   });
}

using CallbackType = std::function<bool(souper::Inst *)>;

bool getGuesses(const std::vector<souper::Inst *> &Inputs, int Width, int LHSCost,
                souper::InstContext &IC, souper::Inst *PrevInst,
                souper::Inst *PrevSlot, int &TooExpensive, PruneFunc prune,
                CallbackType Generate) {
  std::vector<souper::Inst *> unaryHoleUsers;
  findInsts(PrevInst, unaryHoleUsers, [PrevSlot](souper::Inst *I) {
    return I->Ops.size() == 1 && I->Ops[0] == PrevSlot;
  });

  std::vector<souper::Inst::Kind> unaryExclList;
  if (unaryHoleUsers.size() == 1 &&
      (unaryHoleUsers[0]->K == souper::Inst::Ctlz ||
       unaryHoleUsers[0]->K == souper::Inst::Cttz ||
       unaryHoleUsers[0]->K == souper::Inst::BitReverse ||
       unaryHoleUsers[0]->K == souper::Inst::CtPop)) {
    unaryExclList.push_back(souper::Inst::BitReverse);
  }

  // disable generating freeze of freeze
  if (unaryHoleUsers.size() == 1 &&
      unaryHoleUsers[0]->K == souper::Inst::Freeze)
    unaryExclList.push_back(souper::Inst::Freeze);

  std::vector<souper::Inst *> PartialGuesses;

  std::vector<souper::Inst *> Comps(Inputs.begin(), Inputs.end());

  // Conversion Operators
  for (auto Comp : Comps) {
    if (Comp->Width == Width) continue;

    addGuess(Comp, Width, IC, LHSCost, PartialGuesses, TooExpensive);
  }

  souper::Inst *I1 = IC.getReservedInst();
  Comps.push_back(I1);

  // Unary Operators
  for (auto K : UnaryOperators) {
    if (std::find(unaryExclList.begin(), unaryExclList.end(), K) !=
        unaryExclList.end())
      continue;

    if (K != souper::Inst::Freeze && Width <= 1) continue;

    for (auto Comp : Comps) {
      if (K == souper::Inst::BSwap && Width % 16 != 0) continue;

      if (Comp->K == souper::Inst::ReservedInst) {
        auto V = IC.createHole(Width);
        auto N = IC.getInst(K, Width, {V});
        addGuess(N, Width, IC, LHSCost, PartialGuesses, TooExpensive);
        continue;
      }

      if (Comp->Width != Width) continue;

      // Prune: unary operation on constant
      if (Comp->K == souper::Inst::ReservedConst) continue;

      auto N = IC.getInst(K, Width, {Comp});
      addGuess(N, Width, IC, LHSCost, PartialGuesses, TooExpensive);
    }
  }

  // reservedinst and reservedconsts starts with width 0
  souper::Inst *C1 = IC.getReservedConst();
  Comps.push_back(C1);
  souper::Inst *I2 = IC.getReservedInst();
  Comps.push_back(I2);

  for (auto K : BinaryOperators) {
    // PRUNE: i1 is a special case for a number of operators
    if (Width == 1 &&
        (  // these become trivial
            souper::Inst::isDivRem(K) || souper::Inst::isShift(K) ||
            // these canonicalize to "xor"
            K == souper::Inst::Add || K == souper::Inst::Sub ||
            K == souper::Inst::Ne ||
            // canonicalizes to "and"
            K == souper::Inst::Mul ||
            // i1 versions of these do not tend to codegen well
            K == souper::Inst::SAddSat || K == souper::Inst::UAddSat ||
            K == souper::Inst::SSubSat || K == souper::Inst::USubSat ||
            K == souper::Inst::SAddWithOverflow ||
            K == souper::Inst::UAddWithOverflow ||
            K == souper::Inst::SSubWithOverflow ||
            K == souper::Inst::USubWithOverflow ||
            K == souper::Inst::SMulWithOverflow ||
            K == souper::Inst::UMulWithOverflow)) {
      continue;
    }

    for (auto I = Comps.begin(); I != Comps.end(); ++I) {
      // Prune: only one of (mul x, C), (mul C, x) is allowed
      if ((souper::Inst::isCommutative(K) ||
           souper::Inst::isOverflowIntrinsicMain(K) ||
           souper::Inst::isOverflowIntrinsicSub(K)) &&
          (*I)->K == souper::Inst::ReservedConst)
        continue;

      // Prune: I1 should only be the first argument
      if ((*I)->K == souper::Inst::ReservedInst && (*I) != I1) continue;

      // PRUNE: don't try commutative operators both ways
      auto Start = (souper::Inst::isCommutative(K) ||
                    souper::Inst::isOverflowIntrinsicMain(K) ||
                    souper::Inst::isOverflowIntrinsicSub(K))
                       ? I
                       : Comps.begin();
      for (auto J = Start; J != Comps.end(); ++J) {
        // Prune: I2 should only be the second argument
        if ((*J)->K == souper::Inst::ReservedInst && (*J) != I2) continue;

        // PRUNE: never useful to cmp, sub, and, or, xor, div, rem,
        // usub.sat, ssub.sat, ashr, lshr a value against itself
        // Also do it for sub.overflow -- no sense to check for overflow when
        // results = 0
        if ((*I == *J) &&
            (souper::Inst::isCmp(K) || K == souper::Inst::And ||
             K == souper::Inst::Or || K == souper::Inst::Xor ||
             K == souper::Inst::Sub || K == souper::Inst::UDiv ||
             K == souper::Inst::SDiv || K == souper::Inst::SRem ||
             K == souper::Inst::URem || K == souper::Inst::USubSat ||
             K == souper::Inst::SSubSat || K == souper::Inst::AShr ||
             K == souper::Inst::LShr || K == souper::Inst::SSubWithOverflow ||
             K == souper::Inst::USubWithOverflow || K == souper::Inst::SSubO ||
             K == souper::Inst::USubO))
          continue;

        // PRUNE: never operate on two constants
        if ((*I)->K == souper::Inst::ReservedConst &&
            (*J)->K == souper::Inst::ReservedConst)
          continue;

        // see if we need to make a var representing a constant
        // that we don't know yet

        souper::Inst *V1, *V2;
        if (souper::Inst::isCmp(K)) {
          if ((*I)->Width == 0 && (*J)->Width == 0) {
            // TODO: support (cmp hole, hole);
            continue;
          }

          if ((*I)->Width == 0) {
            if ((*I)->K == souper::Inst::ReservedConst) {
              // (cmp const, comp)
              V1 = IC.createSynthesisConstant((*J)->Width,
                                              (*I)->SynthesisConstID);
            } else if ((*I)->K == souper::Inst::ReservedInst) {
              // (cmp hole, comp)
              V1 = IC.createHole((*J)->Width);
            }
          } else {
            V1 = *I;
          }

          if ((*J)->Width == 0) {
            if ((*J)->K == souper::Inst::ReservedConst) {
              // (cmp comp, const)
              V2 = IC.createSynthesisConstant((*I)->Width,
                                              (*J)->SynthesisConstID);
            } else if ((*J)->K == souper::Inst::ReservedInst) {
              // (cmp comp, hole)
              V2 = IC.createHole((*I)->Width);
            }
          } else {
            V2 = *J;
          }
        } else {
          if ((*I)->K == souper::Inst::ReservedConst) {
            // (binop const, comp)
            V1 = IC.createSynthesisConstant(Width, (*I)->SynthesisConstID);
          } else if ((*I)->K == souper::Inst::ReservedInst) {
            // (binop hole, comp)
            V1 = IC.createHole(Width);
          } else {
            V1 = *I;
          }

          if ((*J)->K == souper::Inst::ReservedConst) {
            // (binop comp, const)
            V2 = IC.createSynthesisConstant(Width, (*J)->SynthesisConstID);
          } else if ((*J)->K == souper::Inst::ReservedInst) {
            // (binop comp, hole)
            V2 = IC.createHole(Width);
          } else {
            V2 = *J;
          }
        }

        if (V1->Width != V2->Width) continue;

        if (!(souper::Inst::isCmp(K) ||
              souper::Inst::isOverflowIntrinsicSub(K)) &&
            V1->Width != Width)
          continue;

        // PRUNE: don't synthesize sub x, C since this is covered by add x, -C
        if (K == souper::Inst::Sub && V2->K == souper::Inst::Var &&
            V2->SynthesisConstID != 0)
          continue;

        souper::Inst *N = nullptr;
        if (souper::Inst::isOverflowIntrinsicMain(K)) {
          auto Comp0 = IC.getInst(souper::Inst::getBasicInstrForOverflow(K),
                                  V1->Width, {V1, V2});
          auto Comp1 =
              IC.getInst(souper::Inst::getOverflowComplement(K), 1, {V1, V2});
          auto Orig = IC.getInst(K, V1->Width + 1, {Comp0, Comp1});
          N = IC.getInst(souper::Inst::ExtractValue, V1->Width,
                         {Orig, IC.getConst(llvm::APInt(32, 0))});
        } else if (souper::Inst::isOverflowIntrinsicSub(K)) {
          auto Comp0 = IC.getInst(souper::Inst::getBasicInstrForOverflow(
                                      souper::Inst::getOverflowComplement(K)),
                                  V1->Width, {V1, V2});
          auto Comp1 = IC.getInst(K, 1, {V1, V2});
          auto Orig = IC.getInst(souper::Inst::getOverflowComplement(K),
                                 V1->Width + 1, {Comp0, Comp1});
          N = IC.getInst(souper::Inst::ExtractValue, 1,
                         {Orig, IC.getConst(llvm::APInt(32, 1))});
        } else {
          N = IC.getInst(K, souper::Inst::isCmp(K) ? 1 : Width, {V1, V2});
        }

        addGuess(N, Width, IC, LHSCost, PartialGuesses, TooExpensive);
      }
    }
  }

  // Deal with ternary instructions separately, since some guesses might
  // need two reserved per instruction
  souper::Inst *C2 = IC.getReservedConst();
  Comps.push_back(C2);
  souper::Inst *C3 = IC.getReservedConst();
  Comps.push_back(C3);
  souper::Inst *I3 = IC.getReservedInst();
  Comps.push_back(I3);

  for (auto Op : TernaryOperators) {
    for (auto I : Comps) {
      if (I->K == souper::Inst::ReservedInst && I != I1) continue;
      if (I->K == souper::Inst::ReservedConst && I != C1) continue;

      // (select c, x, y)
      // PRUNE: a select's control input should never be constant
      if (Op == souper::Inst::Select && I->K == souper::Inst::ReservedConst)
        continue;

      // PRUNE: don't generate an i1 using funnel shift
      if (Width == 1 && (Op == souper::Inst::FShr || Op == souper::Inst::FShl))
        continue;

      souper::Inst *V1;
      if (I->K == souper::Inst::ReservedConst) {
        V1 = IC.createSynthesisConstant(Width, I->SynthesisConstID);
      } else if (I->K == souper::Inst::ReservedInst) {
        V1 = IC.createHole(Op == souper::Inst::Select ? 1 : Width);
      } else {
        V1 = I;
      }

      if (Op == souper::Inst::Select && V1->Width != 1) continue;
      if (Op != souper::Inst::Select && V1->Width != Width) continue;

      for (auto J : Comps) {
        if (J->K == souper::Inst::ReservedInst && J != I2) continue;
        if (J->K == souper::Inst::ReservedConst && J != C2) continue;

        souper::Inst *V2;
        if (J->K == souper::Inst::ReservedConst) {
          V2 = IC.createSynthesisConstant(Width, J->SynthesisConstID);
        } else if (J->K == souper::Inst::ReservedInst) {
          V2 = IC.createHole(Width);
        } else {
          V2 = J;
        }

        if (V2->Width != Width) continue;

        for (auto K : Comps) {
          if (K->K == souper::Inst::ReservedInst && K != I3) continue;
          if (K->K == souper::Inst::ReservedConst && K != C3) continue;

          // PRUNE: ter-op c, c, c
          if (I->K == souper::Inst::ReservedConst &&
              J->K == souper::Inst::ReservedConst &&
              K->K == souper::Inst::ReservedConst)
            continue;

          // PRUNE: (select cond, x, x)
          if (Op == souper::Inst::Select && J == K) continue;

          souper::Inst *V3;
          if (K->K == souper::Inst::ReservedConst) {
            V3 = IC.createSynthesisConstant(Width, K->SynthesisConstID);
          } else if (K->K == souper::Inst::ReservedInst) {
            V3 = IC.createHole(Width);
          } else {
            V3 = K;
          }

          if (V2->Width != V3->Width) continue;

          auto N = IC.getInst(Op, Width, {V1, V2, V3});
          addGuess(N, Width, IC, LHSCost, PartialGuesses, TooExpensive);
        }
      }
    }
  }
  sortGuesses(PartialGuesses);
  // FIXME: This is a bit heavy-handed. Find a way to eliminate this sorting.

  for (auto I : PartialGuesses) {
    souper::Inst *JoinedGuess;
    // if it is the first time the function getGuesses() gets called, then
    // leave it as the root and do not plug it to any other insts
    if (!PrevInst)
      JoinedGuess = I;
    else {
      // plugin the new guess I to PrevInst
      std::map<souper::Inst *, souper::Inst *> InstCache;
      JoinedGuess = instJoin(PrevInst, PrevSlot, I, InstCache, IC);
    }

    // get all empty slots from the newly plugged inst
    std::vector<souper::Inst *> CurrSlots;
    getHoles(JoinedGuess, CurrSlots);
    // FIXME: This is inefficient, to do for each symbolic and concrete
    // candidate

    // if no empty slot, then push the guess to the result list
    if (CurrSlots.empty()) {
      std::vector<souper::Inst *> empty;
      if (prune(JoinedGuess, empty)) {
        std::vector<souper::Inst *> ConcreteTypedGuesses;
        addGuess(JoinedGuess, JoinedGuess->Width, IC, LHSCost,
                 ConcreteTypedGuesses, TooExpensive);
        for (auto &&Guess : ConcreteTypedGuesses) {
          if (!Generate(Guess)) {
            return false;
          }
        }
      }
      continue;
    }

    // if there exist empty slots, then call getGuesses() recursively
    // and fill the empty slots
    if (prune(JoinedGuess, CurrSlots)) {
      // TODO: replace this naive hole selection with some better algorithms
      if (!getGuesses(Inputs, CurrSlots.front()->Width, LHSCost, IC,
                      JoinedGuess, CurrSlots.front(), TooExpensive, prune,
                      Generate)) {
        return false;
      }
    }
  }
  return true;
}

souper::Inst *findConst(souper::Inst *I,
                        std::set<const souper::Inst *> &Visited) {
  if (I->K == souper::Inst::Var && I->SynthesisConstID != 0) {
    return I;
  } else {
    for (auto &&Op : I->Ops) {
      if (Visited.find(Op) == Visited.end()) {
        auto Ret = findConst(Op, Visited);
        if (Ret) {
          return Ret;
        }
      }
    }
    Visited.insert(I);
  }
  return nullptr;
}

bool exceeds64Bits(const souper::Inst *I,
                   std::set<const souper::Inst *> &Visited) {
  if (I->Width > 64) {
    return true;
  } else {
    for (auto &&Op : I->Ops) {
      if (Visited.find(Op) == Visited.end()) {
        if (exceeds64Bits(Op, Visited)) {
          return true;
        }
      }
    }
    Visited.insert(I);
  }
  return false;
}

bool canDifferInLSB(SynthesisContext &SC, souper::Inst *RHSGuess) {
  souper::Inst *LHSOne = SC.IC.getConst(llvm::APInt(SC.LHS->Width, 1));
  souper::Inst *NewLHS =
      SC.IC.getInst(souper::Inst::And, SC.LHS->Width, {SC.LHS, LHSOne});
  souper::Inst *RHSOne = SC.IC.getConst(llvm::APInt(RHSGuess->Width, 1));
  souper::Inst *NewRHS =
      SC.IC.getInst(souper::Inst::And, RHSGuess->Width, {RHSGuess, RHSOne});
  // TODO: Experiment with larger masks: 3, 7, MSB, etc.

  souper::InstMapping NewMapping{NewLHS, NewRHS};

  auto Query = BuildQuery(SC.IC, SC.BPCs, SC.PCs, NewMapping, 0, 0);

  bool IsSat;
  auto EC = SolverCache::Get().isSatisfiableCached(Query, IsSat, 0, 0, SC.Timeout);
  if (EC) {
    if (MyDebugLevel > 1) llvm::errs() << "Solver error in LSB pruning!\n";
    return false;
  }
  return IsSat;
}

std::error_code isConcreteCandidateSat(SynthesisContext &SC,
                                       souper::Inst *RHSGuess, bool &IsSat) {
  std::error_code EC;
  souper::InstMapping Mapping(SC.LHS, RHSGuess);

  std::string Query2 = BuildQuery(SC.IC, SC.BPCs, SC.PCs, Mapping, 0, 0);

  EC = SolverCache::Get().isSatisfiableCached(Query2, IsSat, 0, 0, SC.Timeout);
  if (EC && MyDebugLevel > 1) {
    llvm::errs() << "verification query failed!\n";
  }
  return EC;
}

std::error_code synthesizeWithKLEE(SynthesisContext &SC,
                                   std::vector<souper::Inst *> &RHSs,
                                   const std::vector<souper::Inst *> &Guesses) {
  std::error_code EC;

  // find the valid one
  int GuessIndex = -1;

  if (MyDebugLevel > 2) {
    llvm::errs() << "\n-------------------------------------------------\n";
    ReplacementContext Context;
    auto S = GetReplacementLHSString(SC.BPCs, SC.PCs, SC.LHS, Context);
    llvm::errs() << S << "\n";
  }

  for (auto I : Guesses) {
    GuessIndex++;
    if (MyDebugLevel > 2) {
      llvm::errs() << "\n--------------------------------\nguess " << GuessIndex
                   << "\n\n";
      ReplacementContext RC;
      RC.printInst(I, llvm::errs(), /*printNames=*/true);
      llvm::errs() << "\n";
      llvm::errs() << "Cost = "
                   << souper::cost(I, /*IgnoreDepsWithExternalUses=*/true)
                   << "\n";
    }

    souper::Inst *RHS = nullptr;
    std::set<souper::Inst *> ConstSet;
    std::map<souper::Inst *, llvm::APInt> ResultConstMap;
    souper::getConstants(I, ConstSet);
    bool GuessHasConstant = !ConstSet.empty();
    if (!GuessHasConstant) {
      bool IsSAT;

      EC = isConcreteCandidateSat(SC, I, IsSAT);
      if (EC) return EC;
      if (IsSAT) {
        if (MyDebugLevel > 3)
          llvm::errs() << "second query is SAT-- constant doesn't work\n";
        continue;
      } else {
        if (MyDebugLevel > 3) llvm::errs() << "second query is UNSAT\n";
        RHS = I;
      }
    } else {
      // guess has constant(s)
      ConstantSynthesis CS;
      EC = CS.synthesize(
          SC.SMTSolver, SC.BPCs, SC.PCs, souper::InstMapping(SC.LHS, I),
          ConstSet, ResultConstMap, SC.IC, /*MaxTries=*/MaxTries, SC.Timeout,
          /*AvoidNops=*/true);
      if (!ResultConstMap.empty()) {
        std::map<souper::Inst *, souper::Inst *> InstCache;
        std::map<Block *, Block *> BlockCache;
        RHS = getInstCopy(I, SC.IC, InstCache, BlockCache, &ResultConstMap,
                          false, false);
      } else {
        continue;
      }
    }

    assert(RHS);

    if (RHS) {
      RHSs.emplace_back(RHS);
      if (!SC.CheckAllGuesses) return EC;
      if (MyDebugLevel > 3) {
        llvm::outs() << "; result " << RHSs.size() << ":\n";
        ReplacementContext RC;
        RC.printInst(RHS, llvm::outs(), true);
        llvm::outs() << "\n";
      }
    }
  }
  return EC;
}

std::error_code verify(SynthesisContext &SC, std::vector<souper::Inst *> &RHSs,
                       const std::vector<souper::Inst *> &Guesses) {
  std::error_code EC;
  if (SkipSolver || Guesses.empty()) return EC;

  return synthesizeWithKLEE(SC, RHSs, Guesses);
}

std::error_code CustomEnumerativeSynthesis::synthesize(
  const souper::BlockPCs &BPCs, const std::vector<souper::InstMapping> &PCs,
  souper::Inst *LHS, std::vector<souper::Inst *> &RHSs, bool CheckAllGuesses,
    souper::InstContext &IC, unsigned Timeout) {
  if ((OnlyInferI1 || OnlyInferIN) && MaxNumInstructions >= 1)
    llvm::report_fatal_error(
        "Sorry, it is an error to synthesize >= 1 instructions "
        "in integer-only mode");
  if (OnlyInferI1 && OnlyInferIN)
    llvm::report_fatal_error(
        "Sorry, it is an error to specify synthesizing both only "
        "i1 and only iN values");
  SynthesisContext SC{
      IC, SolverCache::Get().getSmtSolver(), LHS, getUBInstCondition(SC.IC, SC.LHS), PCs,
      BPCs, CheckAllGuesses, Timeout
    };
  std::error_code EC;
  std::vector<Inst *> Cands;
  std::set<Inst *> CandsSet;

  findCands(SC.LHS, CandsSet, /*WidthMustMatch=*/false, /*FilterVars=*/false,
            MaxLHSCands);

  for (auto *Cand : CandsSet)
    Cands.push_back(Cand);

  if (MyDebugLevel > 1)
    llvm::errs() << "got " << Cands.size() << " candidates from LHS\n";

  int LHSCost = souper::cost(SC.LHS, /*IgnoreDepsWithExternalUses=*/false);

  if (MyDebugLevel > 1)
    llvm::outs() << "LHSCost: " << LHSCost << "\n";

  int TooExpensive = 0;

  std::vector<souper::Inst *> Inputs;
  findVars(SC.LHS, Inputs);
  PruningManager DataflowPruning(SC, Inputs, MyDebugLevel);

  std::set<souper::Inst *> Visited(Cands.begin(), Cands.end());

  // Cheaper tests go first
  std::vector<PruneFunc> PruneFuncs = {
      [&Visited](souper::Inst *I, std::vector<souper::Inst *> &ReservedInsts) {
        return CountPrune(I, ReservedInsts, Visited);
      }};
  if (EnableDataflowPruning) {
    DataflowPruning.init();
    PruneFuncs.push_back(DataflowPruning.getPruneFunc());
  }
  auto PruneCallback = MkPruneFunc(PruneFuncs);

  std::vector<souper::Inst *> Guesses;

  auto Generate = [&SC, &Guesses, &RHSs, &EC](souper::Inst *Guess) {
    Guesses.push_back(Guess);
    if (Guesses.size() >= MaxV && !SkipSolver) {
      sortGuesses(Guesses);
      EC = verify(SC, RHSs, Guesses);
      Guesses.clear();
      return SC.CheckAllGuesses ||
             (!SC.CheckAllGuesses && RHSs.empty());  // Continue if no RHS
    }
    return true;
  };

  // add constant guess
  // TODO add a poison/undef guess
  if (!(OnlyInferI1 && SC.LHS->Width > 1))
    Guesses.push_back(IC.createSynthesisConstant(SC.LHS->Width, 1));

  // add nop guesses
  if (!OnlyInferI1 && !OnlyInferIN) {
    for (auto I : Cands) {
      if (I->Width == SC.LHS->Width)
        addGuess(I, SC.LHS->Width, SC.IC, LHSCost, Guesses, TooExpensive);
    }
  }

  if (MaxNumInstructions > 0)
    getGuesses(Cands, SC.LHS->Width, LHSCost, SC.IC, nullptr, nullptr,
               TooExpensive, PruneCallback, Generate);

  if (!Guesses.empty() && !SkipSolver) {
    sortGuesses(Guesses);
    EC = verify(SC, RHSs, Guesses);
  }

  if (MyDebugLevel > 1) {
    DataflowPruning.printStats(llvm::errs());
    llvm::errs() << "There are " << Guesses.size() << " Guesses\n";
  }

  // RHSs count, before duplication
  if (MyDebugLevel > 3)
    llvm::errs() << "There are " << RHSs.size()
                 << " RHSs before deduplication\n";

  std::set<souper::Inst *> Dedup(RHSs.begin(), RHSs.end());
  RHSs.assign(Dedup.begin(), Dedup.end());

  // RHSs count, after duplication
  if (MyDebugLevel > 3)
    llvm::errs() << "There are " << RHSs.size()
                 << " RHSs after deduplication\n";

  return EC;
}

};  // namespace synthesis