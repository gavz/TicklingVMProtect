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

#ifndef SATURN_ENUMERATIVE_SYNTHESIS_H
#define SATURN_ENUMERATIVE_SYNTHESIS_H

#include "llvm/ADT/APInt.h"
#include "souper/Extractor/Solver.h"
#include "souper/Inst/Inst.h"

#include <system_error>
#include <utility>
#include <vector>

extern bool UseAlive;
extern unsigned DebugLevel;

namespace synthesis {

class CustomEnumerativeSynthesis {
 public:
  // Synthesize an instruction from the specification in LHS
  std::error_code synthesize(const souper::BlockPCs &BPCs,
                             const std::vector<souper::InstMapping> &PCs,
                             souper::Inst *TargetLHS,
                             std::vector<souper::Inst *> &RHSs,
                             bool CheckAllGuesses, souper::InstContext &IC,
                             unsigned Timeout);
};
}  // namespace synthesis

#endif  // SATURN_ENUMERATIVE_SYNTHESIS_H