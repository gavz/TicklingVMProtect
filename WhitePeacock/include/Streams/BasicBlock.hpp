#pragma once

#include <cinttypes>
#include <cstdint>
#include <set>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

#include <llvm/IR/Function.h>

namespace whitepeacock {

enum class OperandType {
  None,
  Var,
  Imm,
};

enum class BlockType {
  Conditional,
  Unconditional,
  MaybeUnconditional
};

struct Operand {
  OperandType Type = OperandType::None;
  std::string Var;
  uint64_t Imm = 0;

  Operand() = default;
  explicit Operand(std::string var) : Type(OperandType::Var), Var(std::move(var)) {}
  explicit Operand(uint64_t imm) : Type(OperandType::Imm), Imm(imm) {}

  bool operator==(const Operand &other) const {
    if (Type != other.Type)
      return false;
    switch (Type) {
    case OperandType::None:
      return true;
    case OperandType::Var:
      return Var == other.Var;
    case OperandType::Imm:
      return Imm == other.Imm;
    }
    return false;
  }
  bool operator!=(const Operand &other) const { return !(*this == other); }

  std::string toString() const {
    switch (Type) {
    case OperandType::None:
      return "<NONE>";
    case OperandType::Var:
      return Var.c_str();
    case OperandType::Imm: {
      char str[32];
      sprintf(str, "0x%" PRIx64, Imm);
      return str;
    }
    }
    return "";
  }
};

struct Instruction {
  std::string ErrorContext;
  std::string Mnemonic;
  std::vector<Operand> Operands;

  Instruction() = default;
  explicit Instruction(std::string mnemonic, std::vector<Operand> operands = {})
      : Mnemonic(std::move(mnemonic)), Operands(std::move(operands)) {}
  Instruction(std::string mnemonic, uint64_t operandImm)
      : Instruction(std::move(mnemonic), {Operand(operandImm)}) {}
  Instruction(std::string mnemonic, std::string operandVar)
      : Instruction(std::move(mnemonic), {Operand(std::move(operandVar))}) {}

  bool operator==(const Instruction &other) const {
    return Mnemonic == other.Mnemonic && Operands == other.Operands;
  }

  bool operator!=(const Instruction &other) const { return !(*this == other); }

  std::string toString(bool quoteOperands = false) const {
    std::string result;
    result += Mnemonic.c_str();
    for (const auto &operand : Operands) {
      result += " ";
      if (quoteOperands)
        result += "'";
      result += operand.toString();
      if (quoteOperands)
        result += "'";
    }
    return result;
  }
};

struct BasicBlock {
  uint64_t EntryPoint = 0;
  std::vector<Instruction> Instructions;
  std::set<uint64_t> Children;
  llvm::Function *LiftedCode;
  BlockType BlockTy;

  bool operator==(const BasicBlock &other) const {
    return EntryPoint == other.EntryPoint && Instructions == other.Instructions &&
           Children == other.Children;
  }

  bool operator!=(const BasicBlock &other) const { return !(*this == other); }
};

static_assert(std::is_nothrow_move_constructible_v<BasicBlock>,
              "BasicBlock is not trivially move constructible");
static_assert(std::is_nothrow_move_assignable_v<BasicBlock>,
              "BasicBlock is not trivially move assignable");

};