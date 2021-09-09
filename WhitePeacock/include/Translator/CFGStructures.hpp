#pragma once

// triton
#include <triton/instruction.hpp>
#include <triton/register.hpp>

// std
#include <functional>
#include <sstream>
#include <string>
#include <vector>

typedef enum HandlerType {
  INVALID = 0,
  PUSH_VMREG, PUSH_REG, PUSH_REG_SP, PUSH_IMM,
  POP_VMREG, POPF, MUL, IMUL, DIV, IDIV, ADD,
  NOR, NAND, SHL, SHR, SHLD, SHRD, LOAD_SEG,
  STORE_SEG, VM_JUMP, VM_NOP, VM_ENTER, VM_EXIT,
  POP_REG_SP, VM_CPUID, VM_RDTSC, VM_FLDCW
} HandlerType;

typedef std::shared_ptr<triton::arch::Instruction> SharedTritonInstruction;

/**
 * Structure to model a single native instruction (x86/x64 instruction)
 */
typedef struct NativeInstruction {

  std::map<triton::arch::Register, uint64_t> mWrittenRegisters;
  std::map<triton::arch::Register, uint64_t> mReadRegisters;
  SharedTritonInstruction mInstruction;

  // Dump the native instruction to stdout
  void dump() {
    // Dump the read registers
    std::stringstream ss;
    ss << mInstruction << "\n";
    for (const auto& IT : mWrittenRegisters)
      ss << " (W): " << IT.first << " = 0x" << std::hex << IT.second << std::endl;
    for (const auto& IT : mReadRegisters)
      ss << " (R): " << IT.first << " = 0x" << std::hex << IT.second << std::endl;
    // Dump the string
    std::cout << ss.str() << std::endl;
  }

  // Default constructor
  NativeInstruction() {}
  NativeInstruction(SharedTritonInstruction Instruction) : mInstruction(Instruction) {}
} NativeInstruction;

typedef std::shared_ptr<NativeInstruction> SharedNativeInstruction;

/**
 * Structure to model a single virtual instruction (StackVM handler)
 */
typedef struct VirtualInstruction {

  std::vector<SharedNativeInstruction> mUntaintedBody;
  std::vector<SharedNativeInstruction> mTaintedBody;
  std::vector<std::string> mVmBody;
  HandlerType mType;

  // Reset the virtual instruction
  void reset() {
    mUntaintedBody.clear();
    mTaintedBody.clear();
    mVmBody.clear();
  }

  // Print the virtual instructions
  void printVmBody() {
    for (const auto& instruction : mVmBody)
      std::cout << "[VM] " << instruction << std::endl;
  }

  void dumpVMBody(std::stringstream& ss) {
    for (const auto& instruction : mVmBody)
      ss << instruction << std::endl;
  }

  void saveUntaintedInstruction(const SharedNativeInstruction &instruction) {
    mUntaintedBody.push_back(instruction);
  }

  void saveTaintedInstruction(const SharedNativeInstruction &instruction) {
    mTaintedBody.push_back(instruction);
  }
} VirtualInstruction;

typedef std::shared_ptr<VirtualInstruction> SharedVirtualInstruction;

/**
 * Structure to model a single virtual basic block
 */
typedef struct VirtualBasicBlock {

  std::vector<SharedVirtualInstruction> mVmBody;
  uint64_t mAddress;

  // Save a virtual instruction
  void saveInstruction(const SharedVirtualInstruction &vmInstruction) {
    mVmBody.push_back(vmInstruction);
  }

  // Get the address
  uint64_t getAddress() { return mAddress; }

  // Default constructors
  VirtualBasicBlock(uint64_t Address) : mAddress(Address) {
    std::cout << "VirtualBasicBlock(0x" << std::hex << mAddress << ")" << std::endl;
  }
  VirtualBasicBlock() : mAddress(0) {}
} VirtualBasicBlock;

typedef std::shared_ptr<VirtualBasicBlock> SharedVirtualBasicBlock;