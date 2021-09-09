#include <Streams/FileParserStream.hpp>

#include <cinttypes>
#include <exception>
#include <fstream>
#include <sstream>

void FileParserStream::parse(std::istream &stream) {
  *this = {};

  whitepeacock::BasicBlock currentBasicBlock;

  auto saveBasicBlock = [this, &currentBasicBlock]() {
    if (!currentBasicBlock.Instructions.empty()) {
      BasicBlocks.emplace(currentBasicBlock.EntryPoint, std::move(currentBasicBlock));
    }
    currentBasicBlock = {};
  };

  auto parseHex = [](const std::string &str, uint64_t &value) {
    if (str.length() < 3 || str.substr(0, 2) != "0x")
      return false;
    std::stringstream ss(str);
    return (bool)(ss >> std::hex >> value);
  };

  std::string line;
  for (int lineIndex = 1; std::getline(stream, line); lineIndex++) {
    // Trim the CR character
    if (!line.empty() && line.back() == '\r') {
      line.pop_back();
    }
    if (line.empty())
      continue;
    std::stringstream ssLine(line);
    std::string type, rest;
    std::getline(ssLine, type, ':');
    std::getline(ssLine, rest, ':');
    if (type == "LABEL") {
      saveBasicBlock();
      // LABEL:0x1400c62cb
      if (!parseHex(rest, currentBasicBlock.EntryPoint))
        throw std::runtime_error("failed to parse LABEL (line " + std::to_string(lineIndex) + ")");
    } else if (type == "CHILDREN") {
      // CHILDREN:0x1400d7439,0x140121708
      std::stringstream ssChildren(rest);
      std::string childStr;
      while (std::getline(ssChildren, childStr, ',')) {
        uint64_t child = 0;
        if (!parseHex(childStr, child))
          throw std::runtime_error("failed to parse CHILDREN (line " + std::to_string(lineIndex) +
                                   ")");
        currentBasicBlock.Children.insert(child);
      }
    } else if (type == "INSN") {
      // INSN:PUSH_IMM_64 0x0
      // INSN:ADD64
      // INSN:PUSH_REG_64 rax
      // INSN:POP_VMREG_64 vmregs[22]
      std::stringstream ssInstr(rest);
      whitepeacock::Instruction instr;
      instr.ErrorContext = "line " + std::to_string(lineIndex);
      if (std::getline(ssInstr, instr.Mnemonic, ' ')) {
        std::string operandStr;
        while (std::getline(ssInstr, operandStr, ' ')) {
          auto &operand = instr.Operands.emplace_back();
          if (operandStr.substr(0, 2) == "0x") { // Immediate
            operand.Type = whitepeacock::OperandType::Imm;
            if (!parseHex(operandStr, operand.Imm))
              throw std::runtime_error("failed to parse immediate '" + operandStr + "' (line " +
                                       std::to_string(lineIndex) + ")");
          } else { // Variable
            operand.Type = whitepeacock::OperandType::Var;
            operand.Var = std::move(operandStr);
          }
        }
      }
      currentBasicBlock.Instructions.push_back(std::move(instr));
    } else {
      throw std::runtime_error("unexpected unhandled type '" + type + "' (line " +
                               std::to_string(lineIndex) + ")");
    }
  }
  saveBasicBlock();
}

bool FileParserStream::getBasicBlock(uint64_t entryPoint, whitepeacock::BasicBlock &basicBlock) {
  auto bbItr = BasicBlocks.find(entryPoint);
  if (bbItr == BasicBlocks.end())
    return false;
  basicBlock = bbItr->second;
  return true;
}
