#include <Translator/SVMTranslator.hpp>

/*
 * Architecture dependants variables...
 *
 * MSIZE is the max size of registers in bytes
 * For 64 bits it must be 8
 * For 32 bits it must be 4
 */
static triton::arch::register_e STACK_REGISTER;
static triton::arch::register_e PROGRAM_COUNTER;
static triton::arch::x86::instruction_e PUSHF_INS;
static triton::arch::x86::instruction_e POPF_INS;
static uint64_t MSIZE;
static uint64_t BITS;

static inline uint64_t GetVmInstrSizeFirstOpR(const std::shared_ptr<triton::arch::Instruction>& Instruction) {
  return Instruction->operands[0].getConstRegister().getBitSize();
}

static inline uint64_t GetVmInstrSizeFromMov(const std::shared_ptr<triton::arch::Instruction>& Instruction) {
  uint64_t isize = Instruction->operands[0].getConstRegister().getBitSize();
  if (Instruction->getType() == ID_INS_MOVZX)
    isize = isize / 2;
  return isize;
}

static inline uint64_t GetVmInstrSizeFromMovR(const std::shared_ptr<triton::arch::Instruction>& Instruction) {
  uint64_t isize = Instruction->operands[1].getConstRegister().getBitSize();
  if (Instruction->getType() == ID_INS_MOVZX)
    isize = isize / 2;
  return isize;
}

static inline uint64_t GetOp0RegValue(const SharedNativeInstruction& NativeInstruction) {
  // Get the first operand register
  const auto& Register = NativeInstruction->mInstruction->operands[0].getConstRegister();
  // Read the value of the register
  return NativeInstruction->mWrittenRegisters[Register];
}

/*
 * MATCHING RULE FUNCTIONS BELOW
 */

typedef struct MatchEqualOperandRule MatchEqualOperandRule;
typedef struct MatchInstructionRule MatchInstructionRule;
typedef struct MatchOperandRule MatchOperandRule;

typedef enum MatchEqualOperandType {
  IMM_IMM, REG_REG, MEM_MEM,
  REG_MEM_BASE, REG_MEM_INDEX,
  MEM_BASE_REG, MEM_INDEX_REG,
  IMM_MEM_SCALE, IMM_MEM_DISP,
  MEM_SCALE_IMM, MEM_DISP_IMM
} MatchEqualOperandType;

struct MatchOperandRule {
  // The operand type
  triton::arch::operand_e Type;
  // The register id if it's OP_REG
  triton::arch::register_e RegId;
  // The base register id if it's OP_MEM
  triton::arch::register_e BaseId;
  // The index register id if it's OP_MEM
  triton::arch::register_e IndexId;
  // The scale value if it's OP_MEM
  triton::uint64 ScaleVal;
  bool MatchScale;
  // The displacement value if it's OP_MEM
  triton::uint64 DisplacementVal;
  bool MatchDisplacement;
  // The value if it's OP_IMM
  triton::uint64 ImmValLo;
  triton::uint64 ImmValHi;
  bool MatchImmediate;
};

struct MatchInstructionRule {
  // The matched instruction
  SharedNativeInstruction NInst;
  // The instruction types
  set<triton::arch::x86::instruction_e> Types;
  // The instruction operands
  std::vector<MatchOperandRule> Operands;
  // The instruction equal operands
  std::vector<MatchEqualOperandRule> Equalities;
};

struct MatchEqualOperandRule {
  // Incoming matching instruction rule
  std::shared_ptr<MatchInstructionRule> InRule;
  // Index of the incoming operand
  triton::uint64 InOpIndex;
  // Index of the current operand
  triton::uint64 CurrOpIndex;
  // Match type
  MatchEqualOperandType Type;
  // Match equality
  bool Equality;
};

static inline bool MatchEquality(const std::shared_ptr<triton::arch::Instruction> Instruction, const MatchEqualOperandRule& R)
{
  // Fetch the incoming instruction
  const auto IncomingInstruction = R.InRule->NInst->mInstruction;
  // Fetch the incoming operand
  const auto& IncomingOperand = IncomingInstruction->operands[R.InOpIndex];
  // Fetch the current operand
  const auto& CurrentOperand = Instruction->operands[R.CurrOpIndex];
  // We should handle all kinds of matching rules
  switch (R.Type)
  {
    case MEM_MEM:
    case REG_REG:
    case IMM_IMM: {
      if (IncomingOperand != CurrentOperand && R.Equality) {
        return false;
      }
      if (IncomingOperand == CurrentOperand && !R.Equality) {
        return false;
      }
    } break;
    case REG_MEM_BASE: {
      const auto& Register = IncomingOperand.getConstRegister();
      const auto& Base = CurrentOperand.getConstMemory().getConstBaseRegister();
      if (Register != Base && R.Equality) {
        return false;
      }
      if (Register == Base && !R.Equality) {
        return false;
      }
    } break;
    case REG_MEM_INDEX: {
      const auto& Register = IncomingOperand.getConstRegister();
      const auto& Index = CurrentOperand.getConstMemory().getConstIndexRegister();
      if (Register != Index && R.Equality) {
        return false;
      }
      if (Register == Index && !R.Equality) {
        return false;
      }
    } break;
    case MEM_BASE_REG: {
      const auto& Base = IncomingOperand.getConstMemory().getConstBaseRegister();
      const auto& Register = CurrentOperand.getConstRegister();
      if (Register != Base && R.Equality) {
        return false;
      }
      if (Register == Base && !R.Equality) {
        return false;
      }
    } break;
    case MEM_INDEX_REG: {
      const auto& Index = IncomingOperand.getConstMemory().getConstIndexRegister();
      const auto& Register = CurrentOperand.getConstRegister();
      if (Register != Index && R.Equality) {
        return false;
      }
      if (Register == Index && !R.Equality) {
        return false;
      }
    } break;
    case IMM_MEM_SCALE: {
      const auto& Immediate = IncomingOperand.getConstImmediate();
      const auto& Scale = CurrentOperand.getConstMemory().getConstScale();
      if (Immediate != Scale && R.Equality) {
        return false;
      }
      if (Immediate == Scale && !R.Equality) {
        return false;
      }
    } break;
    case IMM_MEM_DISP: {
      const auto& Immediate = IncomingOperand.getConstImmediate();
      const auto& Displacement = CurrentOperand.getConstMemory().getConstDisplacement();
      if (Immediate != Displacement && R.Equality) {
        return false;
      }
      if (Immediate == Displacement && !R.Equality) {
        return false;
      }
    } break;
    case MEM_SCALE_IMM: {
      const auto& Scale = IncomingOperand.getConstMemory().getConstScale();
      const auto& Immediate = CurrentOperand.getConstImmediate();
      if (Immediate != Scale && R.Equality) {
        return false;
      }
      if (Immediate == Scale && !R.Equality) {
        return false;
      }
    } break;
    case MEM_DISP_IMM: {
      const auto& Displacement = IncomingOperand.getConstMemory().getConstDisplacement();
      const auto& Immediate = CurrentOperand.getConstImmediate();
      if (Immediate != Displacement && R.Equality) {
        return false;
      }
      if (Immediate == Displacement && !R.Equality) {
        return false;
      }
    } break;
  }
  // We matched the equality
  return true;
}

static inline bool MatchOperand(const triton::arch::OperandWrapper& O, const MatchOperandRule& R)
{
  // First match the operand type (if necessary)
  if (R.Type != triton::arch::OP_INVALID && R.Type != O.getType()) {
    return false;
  }
  // Then match the operand
  switch (R.Type) {
    case triton::arch::OP_IMM: {
      // Fetch the immediate
      const auto& Immediate = O.getConstImmediate();
      // Fethc the immediate value
      uint64_t Value = Immediate.getValue();
      // Match the value (if necessary)
      if (R.MatchImmediate && (Value < R.ImmValLo || Value > R.ImmValHi)) {
        return false;
      }
    } break;
    case triton::arch::OP_REG: {
      // Fetch the register
      const auto& Register = O.getConstRegister();
      // Match the register (if necessary)
      if (R.RegId != triton::arch::ID_REG_INVALID && Register.getId() != R.RegId) {
        return false;
      }
    } break;
    case triton::arch::OP_MEM: {
      // Fetch the memory
      const auto& Memory = O.getConstMemory();
      // Fetch the base, index, scale and displacement
      const auto& Base = Memory.getConstBaseRegister();
      const auto& Index = Memory.getConstIndexRegister();
      const auto& Scale = Memory.getConstScale();
      const auto& Displacement = Memory.getConstDisplacement();
      // Match the base (if necessary)
      if (R.BaseId != triton::arch::ID_REG_INVALID && Base.getId() != R.BaseId) {
        return false;
      }
      // Match the index (if necessary)
      if (R.IndexId != triton::arch::ID_REG_INVALID && Index.getId() != R.IndexId) {
        return false;
      }
      // Match the scale (if necessary)
      if (R.MatchScale && Scale.getValue() != R.ScaleVal) {
        return false;
      }
      // Match the displacement (if necessary)
      if (R.MatchDisplacement && Displacement.getValue() != R.DisplacementVal) {
        return false;
      }
    } break;
    case triton::arch::OP_INVALID: break;
  }
  // We matched the operand
  return true;
}

static inline bool MatchInstruction(const SharedNativeInstruction &NI, MatchInstructionRule& R)
{
  // Fetch the Triton instruction
  const auto &I = NI->mInstruction;
  // First match the instruction type
  if (R.Types.find((triton::arch::x86::instruction_e)I->getType()) == R.Types.end())
    return false;
  // Then loop and match the operands
  size_t OperandIndex = 0;
  for (const auto& OR : R.Operands) {
    // Match the current operand
    if (!MatchOperand(I->operands[OperandIndex], OR))
      return false;
    // Increment the operand index
    OperandIndex++;
  }
  // Finally loop and match the equalities
  for (const auto& ER : R.Equalities) {
    // Match the current operand equality
    if (!MatchEquality(I, ER))
      return false;
  }
  // Save the matched instruction
  R.NInst = NI;
  // We matched the instruction
  return true;
}

/*
 * IDENTIFICATION FUNCTIONS BELOW
 */

static inline bool IdentifyPUSH_VMREG(SharedVirtualInstruction& Handler)
{
  // movzx ax, byte ptr [rsp + rax]
  // mov rax, qword ptr [rsp + rax]
  MatchInstructionRule R0 = { .Types = { ID_INS_MOV, ID_INS_MOVZX } };
  R0.Operands.push_back({ .Type = triton::arch::OP_REG });
  R0.Operands.push_back({ .Type = triton::arch::OP_MEM, .BaseId = STACK_REGISTER });
  // mov word ptr [rbp], ax
  // mov qword ptr [rbp], rax
  MatchInstructionRule R1 = { .Types = { ID_INS_MOV } };
  R1.Operands.push_back({ .Type = triton::arch::OP_MEM });
  R1.Operands.push_back({ .Type = triton::arch::OP_REG });
  // Initialize the PUSH_VMREG rule
  std::vector<MatchInstructionRule> Rules{ R0, R1 };
  // Keep track of the amount of matched instructions
  bool Matched = false;
  size_t Matches = 0;
  // Loop the handlers' instructions and match them
  for (const auto& I : Handler->mTaintedBody) {
    // Match the current instruction
    if (MatchInstruction(I, Rules[Matches])) {
      Matches++;
    }
    // Check if we matched the entire pattern
    if (Matches == Rules.size()) {
      Matched = true;
      break;
    }
  }
  // Check if we matched the whole pattern
  if (Matched) {
    // Fetch the index register
    auto Index = Rules[0].NInst->mInstruction->operands[1].getConstMemory().getConstIndexRegister();
    // Fetch its value
    uint64_t Value = Rules[0].NInst->mReadRegisters[Index] / MSIZE;
    uint64_t Offset = Rules[1].NInst->mReadRegisters[Index] % MSIZE;
    // Get the PUSH size
    auto isize = GetVmInstrSizeFromMov(Rules[0].NInst->mInstruction);
    // Generate the instruction
    std::stringstream ss;
    ss << "PUSH_VMREG_" << to_string(isize);
    // Handle _LOW or _HIGH for PUSH
    switch (isize) {
      case 8: {
        switch (Offset) {
          case 0: {
            ss << "_LOW";
          } break;
          case 1: {
            ss << "_HIGH";
          } break;
        }
      } break;
      case 16: {
        switch (Offset) {
          case 0: {
            ss << "_LOWLOW";
          } break;
          case 2: {
            ss << "_LOWHIGH";
          } break;
          case 4: {
            ss << "_HIGHLOW";
          } break;
          case 6: {
            ss << "_HIGHHIGH";
          } break;
        }
      } break;
      case 32: {
        switch (Offset) {
          case 0: {
            ss << "_LOW";
          } break;
          case 4: {
            ss << "_HIGH";
          } break;
        }
      } break;
    }
    ss << " vmregs[" << std::dec << Value << "]";
    Handler->mVmBody.push_back(ss.str());
    // Save the handler's type
    Handler->mType = HandlerType::PUSH_VMREG;
    return true;
  }
  // We didn't match the pattern
  return false;
}

static inline bool IdentifyPUSH_REG_SP(Symbolizer *symbolizer, SharedVirtualInstruction& Handler)
{
  // mov rcx, r8
  MatchInstructionRule R0 = { .Types = { ID_INS_MOV } };
  R0.Operands.push_back({ .Type = triton::arch::OP_REG });
  R0.Operands.push_back({ .Type = triton::arch::OP_REG });
  // mov qword ptr [r8], rcx
  // mov dword ptr [r8], ecx
  // mov word ptr [r8], cx
  MatchInstructionRule R1 = { .Types = { ID_INS_MOV } };
  R1.Operands.push_back({ .Type = triton::arch::OP_MEM });
  R1.Operands.push_back({ .Type = triton::arch::OP_REG });
  // Initialize the PUSH_VMREG rule
  std::vector<MatchInstructionRule> Rules{ R0, R1 };
  // Keep track of the amount of matched instructions
  bool Matched = false;
  size_t Matches = 0;
  // Loop the handlers' instructions and match them
  for (const auto& I : Handler->mTaintedBody) {
    // Match the current instruction
    if (MatchInstruction(I, Rules[Matches]))
      Matches++;
    // Check if we matched the entire pattern
    if (Matches == Rules.size()) {
      Matched = true;
      break;
    }
  }
  // Check if we matched the whole pattern
  if (Matched) {
    auto Reg = Rules[0].NInst->mInstruction->operands[0].getConstRegister();
    auto Src = Rules[0].NInst->mInstruction->operands[1].getConstRegister();
    auto Dst = Rules[1].NInst->mInstruction->operands[1].getConstRegister();
    if (Dst.isOverlapWith(Reg)) {
      uint64_t Value = 0;
      for (const auto &TI : Handler->mTaintedBody) {
        if (TI == Rules[0].NInst)
          Value = TI->mReadRegisters[Src];
      }
      if (!Value) {
        std::cout << "[!] Failed to identify the pushed stack address!" << std::endl;
        std::cin.get();
      }
      if (symbolizer->isStackAddress(Value)) {
        Handler->mVmBody.push_back("PUSH_VSP_" + to_string(Dst.getBitSize()));
        // Save the handler's type
        Handler->mType = HandlerType::PUSH_REG_SP;
        return true;
      }
    }
  }
  return false;
}

static inline bool IdentifyPUSH_IMM(SharedVirtualInstruction& Handler)
{
  // Check if we have a single instruction
  if (Handler->mTaintedBody.size() >= 1) {
    for (const auto &I : Handler->mTaintedBody) {
      // Get the imm value
      if (I->mInstruction->getType() == ID_INS_MOV &&
        I->mInstruction->operands[0].getType() == triton::arch::OP_MEM &&
        I->mInstruction->operands[1].getType() == triton::arch::OP_REG) {
        // Fetch 'reg2'
        auto Reg2 = I->mInstruction->operands[1].getRegister();
        // Fetch its value
        uint64_t Value = I->mReadRegisters[Reg2];
        // Fetch the push size
        auto size = I->mInstruction->operands[0].getBitSize();
        // Generate the instruction
        stringstream ss;
        ss << "0x" << hex << Value;
        Handler->mVmBody.push_back("PUSH_IMM_" + to_string(size) + " " + ss.str());
        // Save the handler's type
        Handler->mType = HandlerType::PUSH_IMM;
        return true;
      }
    }
  }
  return false;
}

static inline bool IdentifyPOP_VMREG(SharedVirtualInstruction& Handler)
{
  // mov cx, word ptr [rbp]
  // mov rcx, qword ptr [rbp]
  MatchInstructionRule R0 = { .Types = { ID_INS_MOV, ID_INS_MOVZX } };
  R0.Operands.push_back({ .Type = triton::arch::OP_REG });
  R0.Operands.push_back({ .Type = triton::arch::OP_MEM });
  // mov byte ptr [rsp + rax], cl
  // mov qword ptr [rsp + rax], rcx
  MatchInstructionRule R1 = { .Types = { ID_INS_MOV } };
  R1.Operands.push_back({ .Type = triton::arch::OP_MEM, .BaseId = STACK_REGISTER });
  R1.Operands.push_back({ .Type = triton::arch::OP_REG });
  // Initialize the PUSH_VMREG rule
  std::vector<MatchInstructionRule> Rules{ R0, R1 };
  // Keep track of the amount of matched instructions
  bool Matched = false;
  size_t Matches = 0;
  // Loop the handlers' instructions and match them
  for (const auto& I : Handler->mTaintedBody) {
    // Match the current instruction
    if (MatchInstruction(I, Rules[Matches])) {
      Matches++;
    }
    // Check if we matched the entire pattern
    if (Matches == Rules.size()) {
      Matched = true;
      break;
    }
  }
  // Check if we matched the whole pattern
  if (Matched) {
    // Fetch the index register
    auto Index = Rules[1].NInst->mInstruction->operands[0].getConstMemory().getConstIndexRegister();
    // Fetch its value
    uint64_t Value = Rules[1].NInst->mReadRegisters[Index] / MSIZE;
    uint64_t Offset = Rules[1].NInst->mReadRegisters[Index] % MSIZE;
    // Get the POP size
    auto isize = GetVmInstrSizeFromMovR(Rules[1].NInst->mInstruction);
    // Generate the instruction
    std::stringstream ss;
    ss << "POP_VMREG_" << to_string(isize);
    // Handle _LOW or _HIGH for POP
    switch (isize) {
      case 8: {
        switch (Offset) {
          case 0: {
            ss << "_LOW";
          } break;
          case 1: {
            ss << "_HIGH";
          } break;
        }
      } break;
      case 16: {
        switch (Offset) {
          case 0: {
            ss << "_LOWLOW";
          } break;
          case 2: {
            ss << "_LOWHIGH";
          } break;
          case 4: {
            ss << "_HIGHLOW";
          } break;
          case 6: {
            ss << "_HIGHHIGH";
          } break;
        }
      } break;
      case 32: {
        switch (Offset) {
          case 0: {
            ss << "_LOW";
          } break;
          case 4: {
            ss << "_HIGH";
          } break;
        }
      } break;
    }
    ss << " vmregs[" << std::dec << Value << "]";
    Handler->mVmBody.push_back(ss.str());
    // Save the handler's type
    Handler->mType = HandlerType::POP_VMREG;
    return true;
  }
  // We didn't match the pattern
  return false;
}

static inline bool IdentifyPOPF(SharedVirtualInstruction& Handler)
{
  MatchInstructionRule R0 = { .Types = { ID_INS_PUSH } };
  MatchInstructionRule R1 = { .Types = { POPF_INS } };
  // Initialize the POPF rule
  std::vector<MatchInstructionRule> Rules{ R0, R1 };
  // Keep track of the amount of matched instructions
  bool Matched = false;
  size_t Matches = 0;
  // Loop the handlers' instructions and match them
  for (const auto& I : Handler->mTaintedBody) {
    // Match the current instruction
    if (MatchInstruction(I, Rules[Matches])) {
      Matches++;
    }
    // Check if we matched the entire pattern
    if (Matches == Rules.size()) {
      Matched = true;
      break;
    }
  }
  // Check if we matched the whole pattern
  if (Matched) {
    Handler->mVmBody.push_back("POP_FLAGS");
    // Save the handler's type
    Handler->mType = HandlerType::POPF;
    return true;
  }
  // We didn't match the pattern
  return false;
}

static inline bool IdentifyMUL(SharedVirtualInstruction& Handler)
{
  MatchInstructionRule R0 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R1 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R2 = { .Types = { ID_INS_MUL } };
  MatchInstructionRule R3 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R4 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R5 = { .Types = { PUSHF_INS } };
  MatchInstructionRule R6 = { .Types = { ID_INS_POP } };
  // Initialize the MUL rule
  // std::vector<MatchInstructionRule> Rules{ R0, R1, R2, R3, R4, R5, R6 };
  std::vector<MatchInstructionRule> Rules{ R0, R1, R2, R3, R4 };
  // Keep track of the amount of matched instructions
  bool Matched = false;
  size_t Matches = 0;
  // Loop the handlers' instructions and match them
  for (const auto& I : Handler->mTaintedBody) {
    // Match the current instruction
    if (MatchInstruction(I, Rules[Matches])) {
      Matches++;
    }
    // Check if we matched the entire pattern
    if (Matches == Rules.size()) {
      Matched = true;
      break;
    }
  }
  // Check if we matched the whole pattern
  if (Matched) {
    auto isize = GetVmInstrSizeFirstOpR(Rules[2].NInst->mInstruction);
    Handler->mVmBody.push_back("MUL_" + to_string(isize));
    // Save the handler's type
    Handler->mType = HandlerType::MUL;
    return true;
  }
  // We didn't match the pattern
  return false;
}

static inline bool IdentifyIMUL(SharedVirtualInstruction& Handler)
{
  MatchInstructionRule R0 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R1 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R2 = { .Types = { ID_INS_IMUL } };
  MatchInstructionRule R3 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R4 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R5 = { .Types = { PUSHF_INS } };
  MatchInstructionRule R6 = { .Types = { ID_INS_POP } };
  // Initialize the IMUL rule
  // std::vector<MatchInstructionRule> Rules{ R0, R1, R2, R3, R4, R5, R6 };
  std::vector<MatchInstructionRule> Rules{ R0, R1, R2, R3, R4 };
  // Keep track of the amount of matched instructions
  bool Matched = false;
  size_t Matches = 0;
  // Loop the handlers' instructions and match them
  for (const auto& I : Handler->mTaintedBody) {
    // Match the current instruction
    if (MatchInstruction(I, Rules[Matches])) {
      Matches++;
    }
    // Check if we matched the entire pattern
    if (Matches == Rules.size()) {
      Matched = true;
      break;
    }
  }
  // Check if we matched the whole pattern
  if (Matched) {
    auto isize = GetVmInstrSizeFirstOpR(Rules[2].NInst->mInstruction);
    Handler->mVmBody.push_back("IMUL_" + to_string(isize));
    // Save the handler's type
    Handler->mType = HandlerType::IMUL;
    return true;
  }
  // We didn't match the pattern
  return false;
}

static inline bool IdentifyDIV(SharedVirtualInstruction& Handler)
{
  MatchInstructionRule R0 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R1 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R2 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R3 = { .Types = { ID_INS_DIV } };
  MatchInstructionRule R4 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R5 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R6 = { .Types = { PUSHF_INS } };
  MatchInstructionRule R7 = { .Types = { ID_INS_POP } };
  // Initialize the SHL rule
  // std::vector<MatchInstructionRule> Rules{ R0, R1, R2, R3, R4, R5, R6, R7 };
  std::vector<MatchInstructionRule> Rules{ R0, R1, R2, R3, R4, R5 };
  // Keep track of the amount of matched instructions
  bool Matched = false;
  size_t Matches = 0;
  // Loop the handlers' instructions and match them
  for (const auto& I : Handler->mTaintedBody) {
    // Match the current instruction
    if (MatchInstruction(I, Rules[Matches])) {
      Matches++;
    }
    // Check if we matched the entire pattern
    if (Matches == Rules.size()) {
      Matched = true;
      break;
    }
  }
  // Check if we matched the whole pattern
  if (Matched) {
    auto isize = GetVmInstrSizeFirstOpR(Rules[3].NInst->mInstruction);
    Handler->mVmBody.push_back("DIV_" + to_string(isize));
    // Save the handler's type
    Handler->mType = HandlerType::DIV;
    return true;
  }
  // We didn't match the pattern
  return false;
}

static inline bool IdentifyIDIV(SharedVirtualInstruction& Handler)
{
  MatchInstructionRule R0 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R1 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R2 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R3 = { .Types = { ID_INS_IDIV } };
  MatchInstructionRule R4 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R5 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R6 = { .Types = { PUSHF_INS } };
  MatchInstructionRule R7 = { .Types = { ID_INS_POP } };
  // Initialize the SHL rule
  // std::vector<MatchInstructionRule> Rules{ R0, R1, R2, R3, R4, R5, R6, R7 };
  std::vector<MatchInstructionRule> Rules{ R0, R1, R2, R3, R4, R5 };
  // Keep track of the amount of matched instructions
  bool Matched = false;
  size_t Matches = 0;
  // Loop the handlers' instructions and match them
  for (const auto& I : Handler->mTaintedBody) {
    // Match the current instruction
    if (MatchInstruction(I, Rules[Matches])) {
      Matches++;
    }
    // Check if we matched the entire pattern
    if (Matches == Rules.size()) {
      Matched = true;
      break;
    }
  }
  // Check if we matched the whole pattern
  if (Matched) {
    auto isize = GetVmInstrSizeFirstOpR(Rules[3].NInst->mInstruction);
    Handler->mVmBody.push_back("IDIV_" + to_string(isize));
    // Save the handler's type
    Handler->mType = HandlerType::IDIV;
    return true;
  }
  // We didn't match the pattern
  return false;
}

static inline bool IdentifyADD(SharedVirtualInstruction& Handler)
{
  MatchInstructionRule R0 = { .Types = { ID_INS_MOV, ID_INS_MOVZX } };
  MatchInstructionRule R1 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R2 = { .Types = { ID_INS_ADD } };
  MatchInstructionRule R3 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R4 = { .Types = { PUSHF_INS } };
  MatchInstructionRule R5 = { .Types = { ID_INS_POP } };
  // Initialize the SHL rule
  std::vector<MatchInstructionRule> Rules{ R0, R1, R2, R3, R4, R5 };
  // Keep track of the amount of matched instructions
  bool Matched = false;
  size_t Matches = 0;
  // Loop the handlers' instructions and match them
  for (const auto& I : Handler->mTaintedBody) {
    // Match the current instruction
    if (MatchInstruction(I, Rules[Matches])) {
      Matches++;
    }
    // Check if we matched the entire pattern
    if (Matches == Rules.size()) {
      Matched = true;
      break;
    }
  }
  // Check if we matched the whole pattern
  if (Matched) {
    auto isize = GetVmInstrSizeFromMov(Rules[0].NInst->mInstruction);
    Handler->mVmBody.push_back("ADD_" + to_string(isize));
    // Save the handler's type
    Handler->mType = HandlerType::ADD;
    return true;
  }
  // We didn't match the pattern
  return false;
}

static inline bool IdentifyNOR(SharedVirtualInstruction& Handler)
{
  MatchInstructionRule R0 = { .Types = { ID_INS_MOV, ID_INS_MOVZX } };
  MatchInstructionRule R1 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R2 = { .Types = { ID_INS_NOT } };
  MatchInstructionRule R3 = { .Types = { ID_INS_NOT } };
  MatchInstructionRule R4 = { .Types = { ID_INS_AND } };
  MatchInstructionRule R5 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R6 = { .Types = { PUSHF_INS } };
  MatchInstructionRule R7 = { .Types = { ID_INS_POP } };
  // Initialize the SHL rule
  std::vector<MatchInstructionRule> Rules{ R0, R1, R2, R3, R4, R5, R6, R7 };
  // Keep track of the amount of matched instructions
  bool Matched = false;
  size_t Matches = 0;
  // Loop the handlers' instructions and match them
  for (const auto& I : Handler->mTaintedBody) {
    // Match the current instruction
    if (MatchInstruction(I, Rules[Matches])) {
      Matches++;
    }
    // Check if we matched the entire pattern
    if (Matches == Rules.size()) {
      Matched = true;
      break;
    }
  }
  // Check if we matched the whole pattern
  if (Matched) {
    auto isize = GetVmInstrSizeFromMov(Rules[0].NInst->mInstruction);
    Handler->mVmBody.push_back("NOR_" + to_string(isize));
    // Save the handler's type
    Handler->mType = HandlerType::NOR;
    return true;
  }
  // We didn't match the pattern
  return false;
}

static inline bool IdentifyNAND(SharedVirtualInstruction& Handler)
{
  MatchInstructionRule R0 = { .Types = { ID_INS_MOV, ID_INS_MOVZX } };
  MatchInstructionRule R1 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R2 = { .Types = { ID_INS_NOT } };
  MatchInstructionRule R3 = { .Types = { ID_INS_NOT } };
  MatchInstructionRule R4 = { .Types = { ID_INS_OR } };
  MatchInstructionRule R5 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R6 = { .Types = { PUSHF_INS } };
  MatchInstructionRule R7 = { .Types = { ID_INS_POP } };
  // Initialize the SHL rule
  std::vector<MatchInstructionRule> Rules{ R0, R1, R2, R3, R4, R5, R6, R7 };
  // Keep track of the amount of matched instructions
  bool Matched = false;
  size_t Matches = 0;
  // Loop the handlers' instructions and match them
  for (const auto& I : Handler->mTaintedBody) {
    // Match the current instruction
    if (MatchInstruction(I, Rules[Matches])) {
      Matches++;
    }
    // Check if we matched the entire pattern
    if (Matches == Rules.size()) {
      Matched = true;
      break;
    }
  }
  // Check if we matched the whole pattern
  if (Matched) {
    auto isize = GetVmInstrSizeFromMov(Rules[0].NInst->mInstruction);
    Handler->mVmBody.push_back("NAND_" + to_string(isize));
    // Save the handler's type
    Handler->mType = HandlerType::NAND;
    return true;
  }
  // We didn't match the pattern
  return false;
}

static inline bool IdentifySHL(SharedVirtualInstruction& Handler)
{
  // 0x1403cac59: mov ebp, dword ptr [r8]
  // 0x1403cac5c: mov cl, byte ptr [r8 + 4]
  // 0x1403cac6f: shl ebp, cl
  // 0x1403cac7b: mov dword ptr [r8 + 8], ebp
  MatchInstructionRule R0 = { .Types = { ID_INS_MOV, ID_INS_MOVZX } };
  R0.Operands.push_back({ .Type = triton::arch::OP_REG });
  R0.Operands.push_back({ .Type = triton::arch::OP_MEM });
  MatchInstructionRule R1 = { .Types = { ID_INS_MOV } };
  R1.Operands.push_back({ .Type = triton::arch::OP_REG });
  R1.Operands.push_back({ .Type = triton::arch::OP_MEM });
  MatchInstructionRule R2 = { .Types = { ID_INS_SHL } };
  R2.Operands.push_back({ .Type = triton::arch::OP_REG });
  R2.Operands.push_back({ .Type = triton::arch::OP_REG });
  MatchInstructionRule R3 = { .Types = { ID_INS_MOV } };
  R3.Operands.push_back({ .Type = triton::arch::OP_MEM });
  R3.Operands.push_back({ .Type = triton::arch::OP_REG });
  MatchInstructionRule R4 = { .Types = { PUSHF_INS } };
  MatchInstructionRule R5 = { .Types = { ID_INS_POP } };
  // Initialize the SHL rule
  std::vector<MatchInstructionRule> Rules{ R0, R1, R2, R3 };
  // std::vector<MatchInstructionRule> Rules{ R0, R1, R2, R3, R4, R5 };
  // Keep track of the amount of matched instructions
  bool Matched = false;
  size_t Matches = 0;
  // Loop the handlers' instructions and match them
  for (const auto& I : Handler->mTaintedBody) {
    // Match the current instruction
    if (MatchInstruction(I, Rules[Matches]))
      Matches++;
    // Check if we matched the entire pattern
    if (Matches == Rules.size()) {
      Matched = true;
      break;
    }
  }
  // Check if we matched the whole pattern
  if (Matched) {
    auto isize = GetVmInstrSizeFromMov(Rules[0].NInst->mInstruction);
    Handler->mVmBody.push_back("SHL_" + to_string(isize));
    // Save the handler's type
    Handler->mType = HandlerType::SHL;
    return true;
  }
  // We didn't match the pattern
  return false;
}

static inline bool IdentifySHR(SharedVirtualInstruction& Handler)
{
  // 0x1403cac59: mov ebp, dword ptr [r8]
  // 0x1403cac5c: mov cl, byte ptr [r8 + 4]
  // 0x1403cac6f: shr ebp, cl
  // 0x1403cac7b: mov dword ptr [r8 + 8], ebp
  MatchInstructionRule R0 = { .Types = { ID_INS_MOV, ID_INS_MOVZX } };
  R0.Operands.push_back({ .Type = triton::arch::OP_REG });
  R0.Operands.push_back({ .Type = triton::arch::OP_MEM });
  MatchInstructionRule R1 = { .Types = { ID_INS_MOV } };
  R1.Operands.push_back({ .Type = triton::arch::OP_REG });
  R1.Operands.push_back({ .Type = triton::arch::OP_MEM });
  MatchInstructionRule R2 = { .Types = { ID_INS_SHR } };
  R2.Operands.push_back({ .Type = triton::arch::OP_REG });
  R2.Operands.push_back({ .Type = triton::arch::OP_REG });
  MatchInstructionRule R3 = { .Types = { ID_INS_MOV } };
  R3.Operands.push_back({ .Type = triton::arch::OP_MEM });
  R3.Operands.push_back({ .Type = triton::arch::OP_REG });
  MatchInstructionRule R4 = { .Types = { PUSHF_INS } };
  MatchInstructionRule R5 = { .Types = { ID_INS_POP } };
  // Initialize the SHR rule
  std::vector<MatchInstructionRule> Rules{ R0, R1, R2, R3 };
  // std::vector<MatchInstructionRule> Rules{ R0, R1, R2, R3, R4, R5 };
  // Keep track of the amount of matched instructions
  bool Matched = false;
  size_t Matches = 0;
  // Loop the handlers' instructions and match them
  for (const auto& I : Handler->mTaintedBody) {
    // Match the current instruction
    if (MatchInstruction(I, Rules[Matches])) {
      Matches++;
    }
    // Check if we matched the entire pattern
    if (Matches == Rules.size()) {
      Matched = true;
      break;
    }
  }
  // Check if we matched the whole pattern
  if (Matched) {
    auto isize = GetVmInstrSizeFromMov(Rules[0].NInst->mInstruction);
    Handler->mVmBody.push_back("SHR_" + to_string(isize));
    // Save the handler's type
    Handler->mType = HandlerType::SHR;
    return true;
  }
  // We didn't match the pattern
  return false;
}

static inline bool IdentifySHLD(SharedVirtualInstruction& Handler)
{
  MatchInstructionRule R0 = { .Types = { ID_INS_MOV, ID_INS_MOVZX } };
  R0.Operands.push_back({ .Type = triton::arch::OP_REG });
  R0.Operands.push_back({ .Type = triton::arch::OP_MEM });
  MatchInstructionRule R1 = { .Types = { ID_INS_MOV } };
  R1.Operands.push_back({ .Type = triton::arch::OP_REG });
  R1.Operands.push_back({ .Type = triton::arch::OP_MEM });
  MatchInstructionRule R2 = { .Types = { ID_INS_MOV } };
  R2.Operands.push_back({ .Type = triton::arch::OP_REG });
  R2.Operands.push_back({ .Type = triton::arch::OP_MEM });
  MatchInstructionRule R3 = { .Types = { ID_INS_SHLD } };
  R3.Operands.push_back({ .Type = triton::arch::OP_REG });
  R3.Operands.push_back({ .Type = triton::arch::OP_REG });
  MatchInstructionRule R4 = { .Types = { ID_INS_MOV } };
  R4.Operands.push_back({ .Type = triton::arch::OP_MEM });
  R4.Operands.push_back({ .Type = triton::arch::OP_REG });
  MatchInstructionRule R5 = { .Types = { PUSHF_INS } };
  MatchInstructionRule R6 = { .Types = { ID_INS_POP } };
  // Initialize the SHLD rule
  std::vector<MatchInstructionRule> Rules{ R0, R1, R2, R3, R4 };
  // std::vector<MatchInstructionRule> Rules{ R0, R1, R2, R3, R4, R5, R6 };
  // Keep track of the amount of matched instructions
  bool Matched = false;
  size_t Matches = 0;
  // Loop the handlers' instructions and match them
  for (const auto& I : Handler->mTaintedBody) {
    // Match the current instruction
    if (MatchInstruction(I, Rules[Matches])) {
      Matches++;
    }
    // Check if we matched the entire pattern
    if (Matches == Rules.size()) {
      Matched = true;
      break;
    }
  }
  // Check if we matched the whole pattern
  if (Matched) {
    auto isize = GetVmInstrSizeFromMov(Rules[0].NInst->mInstruction);
    Handler->mVmBody.push_back("SHLD_" + to_string(isize));
    // Save the handler's type
    Handler->mType = HandlerType::SHLD;
    return true;
  }
  // We didn't match the pattern
  return false;
}

static inline bool IdentifySHRD(SharedVirtualInstruction& Handler)
{
  MatchInstructionRule R0 = { .Types = { ID_INS_MOV, ID_INS_MOVZX } };
  R0.Operands.push_back({ .Type = triton::arch::OP_REG });
  R0.Operands.push_back({ .Type = triton::arch::OP_MEM });
  MatchInstructionRule R1 = { .Types = { ID_INS_MOV } };
  R1.Operands.push_back({ .Type = triton::arch::OP_REG });
  R1.Operands.push_back({ .Type = triton::arch::OP_MEM });
  MatchInstructionRule R2 = { .Types = { ID_INS_MOV } };
  R2.Operands.push_back({ .Type = triton::arch::OP_REG });
  R2.Operands.push_back({ .Type = triton::arch::OP_MEM });
  MatchInstructionRule R3 = { .Types = { ID_INS_SHRD } };
  R3.Operands.push_back({ .Type = triton::arch::OP_REG });
  R3.Operands.push_back({ .Type = triton::arch::OP_REG });
  MatchInstructionRule R4 = { .Types = { ID_INS_MOV } };
  R4.Operands.push_back({ .Type = triton::arch::OP_MEM });
  R4.Operands.push_back({ .Type = triton::arch::OP_REG });
  MatchInstructionRule R5 = { .Types = { PUSHF_INS } };
  MatchInstructionRule R6 = { .Types = { ID_INS_POP } };
  // Initialize the SHRD rule
  std::vector<MatchInstructionRule> Rules{ R0, R1, R2, R3, R4 };
  // std::vector<MatchInstructionRule> Rules{ R0, R1, R2, R3, R4, R5, R6 };
  // Keep track of the amount of matched instructions
  bool Matched = false;
  size_t Matches = 0;
  // Loop the handlers' instructions and match them
  for (const auto& I : Handler->mTaintedBody) {
    // Match the current instruction
    if (MatchInstruction(I, Rules[Matches])) {
      Matches++;
    }
    // Check if we matched the entire pattern
    if (Matches == Rules.size()) {
      Matched = true;
      break;
    }
  }
  // Check if we matched the whole pattern
  if (Matched) {
    auto isize = GetVmInstrSizeFromMov(Rules[0].NInst->mInstruction);
    Handler->mVmBody.push_back("SHRD_" + to_string(isize));
    // Save the handler's type
    Handler->mType = HandlerType::SHRD;
    return true;
  }
  // We didn't match the pattern
  return false;
}

static inline bool IdentifyLOAD_SEG(Symbolizer *symbolizer, SharedVirtualInstruction& Handler)
{
  // Initialize the instruction/operand/equality rules
  auto R0 = make_shared<MatchInstructionRule>();
  auto R1 = make_shared<MatchInstructionRule>();
  auto R2 = make_shared<MatchInstructionRule>();
  *R0 = (MatchInstructionRule){ .Types = { ID_INS_MOV } };
  *R1 = (MatchInstructionRule){ .Types = { ID_INS_MOV, ID_INS_MOVZX } };
  R1->Operands.push_back({ .Type = triton::arch::OP_REG });
  R1->Operands.push_back({ .Type = triton::arch::OP_MEM });
  R1->Equalities.push_back({ .InRule = R0, .InOpIndex = 0, .CurrOpIndex = 1, .Type = REG_MEM_BASE, .Equality = true });
  *R2 = (MatchInstructionRule){ .Types = { ID_INS_MOV } };
  R2->Equalities.push_back({ .InRule = R1, .InOpIndex = 0, .CurrOpIndex = 1, .Type = REG_REG, .Equality = true });
  // Load the rules
  std::vector<std::shared_ptr<MatchInstructionRule>> Rules{R0, R1, R2};
  // Keep track of the amount of matched instructions
  bool Matched = false;
  size_t Matches = 0;
  // Loop the handlers' instructions and match them
  for (const auto& I : Handler->mTaintedBody) {
    // Match the current instruction
    if (MatchInstruction(I, *Rules[Matches])) {
      Matches++;
    }
    // Check if we matched the entire pattern
    if (Matches == Rules.size()) {
      Matched = true;
      break;
    }
  }
  // Check if we matched the whole pattern
  if (Matched) {
    auto isize = GetVmInstrSizeFromMov(Rules[1]->NInst->mInstruction);
    auto segmentId = Rules[1]->NInst->mInstruction->operands[1].getConstMemory().getConstSegmentRegister().getId();
    if (segmentId == triton::arch::ID_REG_X86_SS) {
      Handler->mVmBody.push_back("LOAD_SS_" + to_string(isize));
    } else if (segmentId == triton::arch::ID_REG_X86_CS) {
      Handler->mVmBody.push_back("LOAD_CS_" + to_string(isize));
    } else if (segmentId == triton::arch::ID_REG_X86_FS) {
      Handler->mVmBody.push_back("LOAD_FS_" + to_string(isize));
    } else if (segmentId == triton::arch::ID_REG_X86_GS) {
      Handler->mVmBody.push_back("LOAD_GS_" + to_string(isize));
    } else if (segmentId == triton::arch::ID_REG_X86_ES) {
      Handler->mVmBody.push_back("LOAD_ES_" + to_string(isize));
    } else if (segmentId == triton::arch::ID_REG_X86_DS) {
      Handler->mVmBody.push_back("LOAD_DS_" + to_string(isize));
    } else {
      // Get the address used during the load
      uint64_t LoadAddress = GetOp0RegValue(Rules[0]->NInst);
      // The segment is not specified, we can try to detect it nonetheless
      if (symbolizer->isStackAddress(LoadAddress)) {
        Handler->mVmBody.push_back("LOAD_SS_" + to_string(isize));
      } else {
        Handler->mVmBody.push_back("LOAD_DS_" + to_string(isize));
      }
    }
    // Save the handler's type
    Handler->mType = HandlerType::LOAD_SEG;
    return true;
  }
  // We didn't match the pattern
  return false;
}

static inline bool IdentifySTORE_SEG(Symbolizer *symbolizer, SharedVirtualInstruction& Handler)
{
  // Initialize the instruction/operand/equality rules
  auto R0 = make_shared<MatchInstructionRule>();
  auto R1 = make_shared<MatchInstructionRule>();
  auto R2 = make_shared<MatchInstructionRule>();
  *R0 = (MatchInstructionRule){ .Types = { ID_INS_MOV } };
  *R1 = (MatchInstructionRule){ .Types = { ID_INS_MOV } };
  R1->Operands.push_back({ .Type = triton::arch::OP_REG });
  R1->Operands.push_back({ .Type = triton::arch::OP_MEM, .DisplacementVal = MSIZE, .MatchDisplacement = true });
  *R2 = (MatchInstructionRule){ .Types = { ID_INS_MOV } };
  R2->Equalities.push_back({ .InRule = R0, .InOpIndex = 0, .CurrOpIndex = 0, .Type = REG_MEM_BASE, .Equality = true });
  R2->Equalities.push_back({ .InRule = R1, .InOpIndex = 0, .CurrOpIndex = 1, .Type = REG_REG, .Equality = true });
  // Load the rules
  std::vector<std::shared_ptr<MatchInstructionRule>> Rules{R0, R1, R2};
  // Keep track of the amount of matched instructions
  bool Matched = false;
  size_t Matches = 0;
  // Loop the handlers' instructions and match them
  for (const auto& I : Handler->mTaintedBody) {
    // Match the current instruction
    if (MatchInstruction(I, *Rules[Matches])) {
      Matches++;
    }
    // Check if we matched the entire pattern
    if (Matches == Rules.size()) {
      Matched = true;
      break;
    }
  }
  // Check if we matched the whole pattern
  if (Matched) {
    auto isize = GetVmInstrSizeFromMov(Rules[1]->NInst->mInstruction);
    auto segmentId = Rules[2]->NInst->mInstruction->operands[0].getConstMemory().getConstSegmentRegister().getId();
    if (segmentId == triton::arch::ID_REG_X86_SS) {
      Handler->mVmBody.push_back("STORE_SS_" + to_string(isize));
    } else if (segmentId == triton::arch::ID_REG_X86_CS) {
      Handler->mVmBody.push_back("STORE_CS_" + to_string(isize));
    } else if (segmentId == triton::arch::ID_REG_X86_FS) {
      Handler->mVmBody.push_back("STORE_FS_" + to_string(isize));
    } else if (segmentId == triton::arch::ID_REG_X86_GS) {
      Handler->mVmBody.push_back("STORE_GS_" + to_string(isize));
    } else if (segmentId == triton::arch::ID_REG_X86_ES) {
      Handler->mVmBody.push_back("STORE_ES_" + to_string(isize));
    } else if (segmentId == triton::arch::ID_REG_X86_DS) {
      Handler->mVmBody.push_back("STORE_DS_" + to_string(isize));
    } else {
      // Get the address used during the store
      uint64_t StoreAddress = GetOp0RegValue(Rules[0]->NInst);
      // The segment is not specified, we can try to detect it nonetheless
      if (symbolizer->isStackAddress(StoreAddress)) {
        Handler->mVmBody.push_back("STORE_SS_" + to_string(isize));
      } else {
        Handler->mVmBody.push_back("STORE_DS_" + to_string(isize));
      }
    }
    // Save the handler's type
    Handler->mType = HandlerType::STORE_SEG;
    return true;
  }
  // We didn't match the pattern
  return false;
}

static inline bool IdentifyVM_JUMP_v1(SharedVirtualInstruction& Handler)
{
  // 0x1403B670E mov rbx, [r8]
  // 0x1403B672C mov rsi, rbx
  // 0x14032BEE1 lea r11, [rip - 7]
  // 0x14032BEFE mov ecx, [rsi]
  MatchInstructionRule R0 = { .Types = { ID_INS_MOV } };
  R0.Operands.push_back({ .Type = triton::arch::OP_REG });
  R0.Operands.push_back({ .Type = triton::arch::OP_MEM });
  MatchInstructionRule R1 = { .Types = { ID_INS_MOV } };
  R1.Operands.push_back({ .Type = triton::arch::OP_REG });
  R1.Operands.push_back({ .Type = triton::arch::OP_REG });
  MatchInstructionRule R2 = { .Types = { ID_INS_LEA } };
  R2.Operands.push_back({ .Type = triton::arch::OP_REG });
  R2.Operands.push_back({ .Type = triton::arch::OP_MEM });
  MatchInstructionRule R3 = { .Types = { ID_INS_MOV } };
  R3.Operands.push_back({ .Type = triton::arch::OP_REG });
  R3.Operands.push_back({ .Type = triton::arch::OP_MEM });
  // Initialize the VM_JUMP rule
  std::vector<MatchInstructionRule> Rules{ R0, R1, R2, R3 };
  // Keep track of the amount of matched instructions
  bool Matched = false;
  size_t Matches = 0;
  // Loop the handlers' instructions and match them
  bool Forward = true;
  size_t CallCount = 0;
  for (const auto& I : Handler->mTaintedBody) {
    // Match the current instruction
    if (MatchInstruction(I, Rules[Matches]))
      Matches++;
    // Check if we found a CALL
    if (I->mInstruction->getType() == ID_INS_CALL)
      CallCount++;
    // Check if we matched the entire pattern
    if (Matches == Rules.size()) {
      Matched = true;
      break;
    }
    // Save the tainted add/sub instruction
    const auto &TI = I->mInstruction;
    if (TI->getType() == ID_INS_ADD &&
      TI->operands[0].getType() == triton::arch::OP_REG &&
      TI->operands[1].getType() == triton::arch::OP_IMM)
    {
      int64_t Value = (int64_t)TI->operands[1].getConstImmediate().getValue();
      if (Value < 0)
        Forward = false;
    } else if (TI->getType() == ID_INS_SUB &&
      TI->operands[0].getType() == triton::arch::OP_REG &&
      TI->operands[1].getType() == triton::arch::OP_IMM) {
      int64_t Value = (int64_t)TI->operands[1].getConstImmediate().getValue();
      if (Value > 0)
        Forward = false;
    }
  }
  // Check if we matched the whole pattern
  if (Matched && CallCount == 0) {
    // Identify if it's a forward or backward bytecode
    if (Forward)
      Handler->mVmBody.push_back("JUMP_INC");
    else
      Handler->mVmBody.push_back("JUMP_DEC");
    // Save the handler's type
    Handler->mType = HandlerType::VM_JUMP;
    return true;
  }
  // We didn't match the pattern
  return false;
}

static inline bool IdentifyVM_JUMP_v2(SharedVirtualInstruction& Handler)
{
  // 0x817dc29: mov edi, dword ptr [ebp]
  // 0x817dc38: mov esi, edi
  // 0x817f35f: movzx ecx, byte ptr [esi]
  MatchInstructionRule R0 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R1 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R2 = { .Types = { ID_INS_MOVZX } };
  // Initialize the VM_JUMP rule
  std::vector<MatchInstructionRule> Rules{ R0, R1, R2 };
  // Keep track of the amount of matched instructions
  bool Matched = false;
  size_t Matches = 0;
  // Loop the handlers' instructions and match them
  bool Forward = true;
  for (const auto& I : Handler->mTaintedBody) {
    // Match the current instruction
    if (MatchInstruction(I, Rules[Matches])) {
      Matches++;
    }
    // Check if we matched the entire pattern
    if (Matches == Rules.size()) {
      Matched = true;
      break;
    }
    // Save the tainted add/sub instruction
    const auto &TI = I->mInstruction;
    if (TI->getType() == ID_INS_ADD &&
      TI->operands[0].getType() == triton::arch::OP_REG &&
      TI->operands[1].getType() == triton::arch::OP_IMM)
    {
      int64_t Value = (int64_t)TI->operands[1].getConstImmediate().getValue();
      if (Value < 0)
        Forward = false;
    } else if (TI->getType() == ID_INS_SUB &&
      TI->operands[0].getType() == triton::arch::OP_REG &&
      TI->operands[1].getType() == triton::arch::OP_IMM) {
      int64_t Value = (int64_t)TI->operands[1].getConstImmediate().getValue();
      if (Value > 0)
        Forward = false;
    }
  }
  // Check if we matched the whole pattern
  if (Matched) {
    // Identify if it's a forward or backward bytecode
    if (Forward)
      Handler->mVmBody.push_back("JUMP_INC");
    else
      Handler->mVmBody.push_back("JUMP_DEC");
    // Save the handler's type
    Handler->mType = HandlerType::VM_JUMP;
    return true;
  }
  // We didn't match the pattern
  return false;
}

static inline bool IdentifyVM_NOP(SharedVirtualInstruction& Handler)
{
  if (Handler->mTaintedBody.size() >= 1) {
    int nbmov = 0;
    int nblea = 0;
    int nbcall = 0;
    for (const auto& HI : Handler->mTaintedBody) {
      const auto& I = HI->mInstruction;
      if (I->getType() == ID_INS_CALL) {
        nbcall++;
      } else if (I->getType() == ID_INS_MOV && I->operands[0].getType() == triton::arch::OP_REG && I->operands[1].getType() == triton::arch::OP_MEM) {
        nbmov++;
      } else if (I->getType() == ID_INS_LEA && I->operands[0].getType() == triton::arch::OP_REG && I->operands[1].getType() == triton::arch::OP_MEM) {
        auto Memory = I->operands[1].getMemory();
        auto Base = Memory.getBaseRegister();
        auto Index = Memory.getIndexRegister();
        if (Base.getName() == "unknown" && Index.getName() == "unknown") {
          nblea++;
        } else if (Base.getName() == "rip") {
          nblea++;
        }
      }
    }
    if (nbcall == 0 && nblea == 1) {

      uint64_t vipValue = 0;
      triton::arch::Register vipRegister;
      std::vector<SharedTritonInstruction> addSub;
      for (const auto &I : Handler->mUntaintedBody) {
        const auto &TI = I->mInstruction;
        if (TI->getType() == ID_INS_MOV &&
            TI->operands[0].getType() == triton::arch::OP_REG &&
            TI->operands[1].getType() == triton::arch::OP_MEM)
        {
          vipRegister = TI->operands[1].getConstMemory().getConstBaseRegister();
          vipValue = I->mReadRegisters[vipRegister];
        } else if ((TI->getType() == ID_INS_ADD || TI->getType() == ID_INS_SUB) &&
          TI->operands[0].getType() == triton::arch::OP_REG &&
          TI->operands[1].getType() == triton::arch::OP_IMM)
        {
          addSub.push_back(TI);
        }
      }

      bool forward = true;
      for (const auto &I : addSub) {
        if (I->operands[0].getConstRegister() == vipRegister) {
          int64_t offset = (int64_t)I->operands[1].getConstImmediate().getValue();
          if (I->getType() == ID_INS_SUB && offset > 0) {
            vipValue += offset;
            forward = false;
          } else if (I->getType() == ID_INS_ADD && offset < 0) {
            vipValue += offset;
            forward = false;
          }
          break;
        }
      }

      stringstream ss;
      ss << "0x" << hex << vipValue;
      Handler->mVmBody.push_back("PUSH_IMM_" + to_string(BITS) + " " + ss.str());
      if (forward)
        Handler->mVmBody.push_back("JUMP_INC");
      else
        Handler->mVmBody.push_back("JUMP_DEC");
      // Save the handler's type
      Handler->mType = HandlerType::VM_NOP;
      return true;
    }
  }
  return false;
}

static inline bool IdentifyVM_ENTER(SharedVirtualInstruction& Handler)
{
  uint64_t count_push = 0;
  uint64_t count_pushf = 0;
  for (const auto& HI : Handler->mTaintedBody) {
    // Count number of PUSH
    if (HI->mInstruction->getType() == ID_INS_PUSH) {
      // Check if the first operand is a register
      if (HI->mInstruction->operands[0].getType() == triton::arch::OP_REG) {
        // Fetch the register
        auto Register = HI->mInstruction->operands[0].getRegister();
        // Add the push register instruction
        Handler->mVmBody.push_back("PUSH_REG_" + to_string(BITS) + " " + Register.getName());
        count_push++;
      }
    }
    // Count number of PUSHF (D or Q 32 vs 64bits)
    else if (HI->mInstruction->getType() == PUSHF_INS) {
      // Add the push flags instruction
      Handler->mVmBody.push_back("PUSH_REG_" + to_string(BITS) + " flags");
      count_pushf++;
    }
    // Determine if we found all the pushes
    if (BITS == 32 && count_push == 7 && count_pushf == 1) {
      // Add the missing instructions
      Handler->mVmBody.insert(Handler->mVmBody.begin(), "PUSH_IMM_" + to_string(BITS) + " RET_ADDR");
      Handler->mVmBody.insert(Handler->mVmBody.begin(), "PUSH_IMM_" + to_string(BITS) + " KEY_STUB");
      Handler->mVmBody.push_back("PUSH_IMM_" + to_string(BITS) + " REL_ADDR");
      // Save the handler's type
      Handler->mType = HandlerType::VM_ENTER;
      return true;
    } else if (BITS == 64 && count_push == 15 && count_pushf == 1) {
      // Add the missing instructions
      Handler->mVmBody.insert(Handler->mVmBody.begin(), "PUSH_IMM_" + to_string(BITS) + " RET_ADDR");
      Handler->mVmBody.insert(Handler->mVmBody.begin(), "PUSH_IMM_" + to_string(BITS) + " KEY_STUB");
      Handler->mVmBody.push_back("PUSH_IMM_" + to_string(BITS) + " REL_ADDR");
      // Save the handler's type
      Handler->mType = HandlerType::VM_ENTER;
      return true;
    }
  }
  // Clear the handler body
  Handler->mVmBody.clear();
  return false;
}

static inline bool IdentifyVM_EXIT(SharedVirtualInstruction& Handler)
{
  uint64_t count_pop = 0;
  uint64_t count_popf = 0;
  for (const auto& HI : Handler->mTaintedBody) {
    // Count number of POP
    if (HI->mInstruction->getType() == ID_INS_POP) {
      // Check if the first operand is a register
      if (HI->mInstruction->operands[0].getType() == triton::arch::OP_REG) {
        // Fetch the register
        auto Register = HI->mInstruction->operands[0].getRegister();
        // Add the pop register instruction
        Handler->mVmBody.push_back("POP_REG_" + to_string(BITS) + " " + Register.getName());
        count_pop++;
      }
    }

    // Count number of PUSHF (D or Q 32 vs 64bits)
    else if (HI->mInstruction->getType() == ID_INS_POPFD || HI->mInstruction->getType() == ID_INS_POPFQ) {
      // Add the pop register instruction
      Handler->mVmBody.push_back("POP_REG_" + to_string(BITS) + " flags");
      count_popf++;
    }
  }

  // Check if we reached enough POP and POPF to consider it a VM_EXIT
  if (count_pop >= 7 && count_popf == 1) {
    Handler->mVmBody.push_back("EXIT");
    // Save the handler's type
    Handler->mType = HandlerType::VM_EXIT;
    return true;
  }

  // We failed to find anything, clear the handler body
  Handler->mVmBody.clear();
  return false;
}

static inline bool IdentifyPOP_REG_SP(SharedVirtualInstruction& Handler)
{
  const auto firstIns = Handler->mTaintedBody[0]->mInstruction;
  if (firstIns->getType() == ID_INS_MOV &&
      firstIns->operands[1].getType() == triton::arch::OP_MEM &&
      firstIns->operands[0].getConstRegister().getId() == firstIns->operands[1].getConstMemory().getConstBaseRegister().getId() &&
      Handler->mTaintedBody.size() >= 1) {
      Handler->mVmBody.push_back("POP_VSP_" + to_string(firstIns->operands[0].getConstRegister().getBitSize()));
      // Save the handler's type
      Handler->mType = HandlerType::POP_REG_SP;
      return true;
  }
  return false;
}

static inline bool IdentifyCPUID(SharedVirtualInstruction& Handler)
{
  MatchInstructionRule R0 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R1 = { .Types = { ID_INS_CPUID } };
  MatchInstructionRule R2 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R3 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R4 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R5 = { .Types = { ID_INS_MOV } };
  // Initialize the CPUID rule
  std::vector<MatchInstructionRule> Rules{ R0, R1, R2, R3, R4, R5 };
  // Keep track of the amount of matched instructions
  bool Matched = false;
  size_t Matches = 0;
  // Loop the handlers' instructions and match them
  for (const auto& I : Handler->mTaintedBody) {
    // Match the current instruction
    if (MatchInstruction(I, Rules[Matches])) {
      Matches++;
    }
    // Check if we matched the entire pattern
    if (Matches == Rules.size()) {
      Matched = true;
      break;
    }
  }
  // Check if we matched the whole pattern
  if (Matched) {
    Handler->mVmBody.push_back("CPUID");
    // Save the handler's type
    Handler->mType = HandlerType::VM_CPUID;
    return true;
  }
  // We didn't match the pattern
  return false;
}

static inline bool IdentifyRDTSC(SharedVirtualInstruction& Handler)
{
  MatchInstructionRule R0 = { .Types = { ID_INS_RDTSC } };
  MatchInstructionRule R1 = { .Types = { ID_INS_MOV } };
  MatchInstructionRule R2 = { .Types = { ID_INS_MOV } };
  R2.Operands.push_back({ .Type = triton::arch::OP_MEM, .DisplacementVal = 4, .MatchDisplacement = true });
  // Initialize the RDTSC rule
  std::vector<MatchInstructionRule> Rules{ R0, R1, R2 };
  // Keep track of the amount of matched instructions
  bool Matched = false;
  size_t Matches = 0;
  // Loop the handlers' instructions and match them
  for (const auto& I : Handler->mTaintedBody) {
    // Match the current instruction
    if (MatchInstruction(I, Rules[Matches])) {
      Matches++;
    }
    // Check if we matched the entire pattern
    if (Matches == Rules.size()) {
      Matched = true;
      break;
    }
  }
  // Check if we matched the whole pattern
  if (Matched) {
    Handler->mVmBody.push_back("RDTSC");
    // Save the handler's type
    Handler->mType = HandlerType::VM_RDTSC;
    return true;
  }
  // We didn't match the pattern
  return false;
}

static inline bool IdentifyFLDCW(SharedVirtualInstruction& Handler)
{
  MatchInstructionRule R0 = { .Types = { ID_INS_FLDCW } };
  // Initialize the CPUID rule
  std::vector<MatchInstructionRule> Rules{ R0 };
  // Keep track of the amount of matched instructions
  bool Matched = false;
  size_t Matches = 0;
  // Loop the handlers' instructions and match them
  for (const auto& I : Handler->mTaintedBody) {
    // Match the current instruction
    if (MatchInstruction(I, Rules[Matches])) {
      Matches++;
    }
    // Check if we matched the entire pattern
    if (Matches == Rules.size()) {
      Matched = true;
      break;
    }
  }
  // Check if we matched the whole pattern
  if (Matched) {
    Handler->mVmBody.push_back("FLDCW");
    // Save the handler's type
    Handler->mType = HandlerType::VM_FLDCW;
    return true;
  }
  // We didn't match the pattern
  return false;
}

bool IdentifyHandler(Symbolizer *symbolizer, SharedVirtualInstruction &vmInstruction, bool is64Bit)
{
  /* First initialize the multi-arch variables */
  if (is64Bit) {
    MSIZE = 8;
    STACK_REGISTER = triton::arch::ID_REG_X86_RSP;
    PROGRAM_COUNTER = triton::arch::ID_REG_X86_RIP;
    PUSHF_INS = ID_INS_PUSHFQ;
    POPF_INS = ID_INS_POPFQ;
    BITS = 64;
  } else {
    MSIZE = 4;
    STACK_REGISTER = triton::arch::ID_REG_X86_ESP;
    PROGRAM_COUNTER = triton::arch::ID_REG_X86_EIP;
    PUSHF_INS = ID_INS_PUSHFD;
    POPF_INS = ID_INS_POPFD;
    BITS = 32;
  }

  /* Checks are by family/in order of frequency */
  if (
      !IdentifyCPUID(vmInstruction) &&
      !IdentifyRDTSC(vmInstruction) &&
      !IdentifyFLDCW(vmInstruction) &&

      !IdentifyPUSH_VMREG(vmInstruction) &&
      !IdentifyPOP_VMREG(vmInstruction) &&

      !IdentifySTORE_SEG(symbolizer, vmInstruction) &&
      !IdentifyLOAD_SEG(symbolizer, vmInstruction) &&

      !IdentifyMUL(vmInstruction) &&
      !IdentifyIMUL(vmInstruction) &&
      !IdentifyDIV(vmInstruction) &&
      !IdentifyIDIV(vmInstruction) &&
      !IdentifyADD(vmInstruction) &&
      !IdentifyNAND(vmInstruction) &&
      !IdentifyNOR(vmInstruction) &&
      !IdentifySHL(vmInstruction) &&
      !IdentifySHR(vmInstruction) &&
      !IdentifySHLD(vmInstruction) &&
      !IdentifySHRD(vmInstruction) &&

      !IdentifyVM_JUMP_v1(vmInstruction) &&
      !IdentifyVM_JUMP_v2(vmInstruction) &&
      !IdentifyVM_NOP(vmInstruction) &&
      !IdentifyVM_ENTER(vmInstruction) &&
      !IdentifyVM_EXIT(vmInstruction) &&
      !IdentifyPOP_REG_SP(vmInstruction) &&
      !IdentifyPUSH_REG_SP(symbolizer, vmInstruction) &&
      !IdentifyPUSH_IMM(vmInstruction) &&
      !IdentifyPOPF(vmInstruction) &&

      true)
  {
    std::cout << "-------------------------" << std::endl;
    for (const auto& NI : vmInstruction->mTaintedBody)
      std::cout << NI->mInstruction << std::endl;
    std::cout << "[VM] [-] Unknown VM Handler..." << std::endl;
    std::cout << "-------------------------" << std::endl;
    return false;
  } else {
    vmInstruction->printVmBody();
    return true;
  }
}
