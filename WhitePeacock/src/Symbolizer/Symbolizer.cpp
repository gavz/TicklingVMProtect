#include <Symbolizer/Symbolizer.hpp>

Symbolizer::Symbolizer(const llvm::object::ObjectFile *BinaryFile, bool Verbose) :
  mBinaryFile(BinaryFile), mVerbose(Verbose) {
  // Determine the image base and bitness
  mImageBase = getImageBase();
  mIs64Bit = is64Bit();
  // Initialize Triton
  mApi = new API();
  mApi->setArchitecture(mIs64Bit ? triton::arch::ARCH_X86_64 : triton::arch::ARCH_X86);
  mApi->setMode(triton::modes::ALIGNED_MEMORY, true);
  mApi->setMode(triton::modes::AST_OPTIMIZATIONS, false);
  mApi->setMode(triton::modes::PC_TRACKING_SYMBOLIC, false);
  mApi->setMode(triton::modes::TAINT_THROUGH_POINTERS, true);
  mApi->setMode(triton::modes::SYMBOLIZE_INDEX_ROTATION, false);
  // Toggle the taint engine
  mApi->enableTaintEngine(true);
}

Symbolizer::~Symbolizer() { delete mApi; }

void Symbolizer::setup(uint64_t XIPV) {
  // Determine the proper registers
  auto XAX = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RAX : triton::arch::register_e::ID_REG_X86_EAX;
  auto XBX = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RBX : triton::arch::register_e::ID_REG_X86_EBX;
  auto XCX = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RCX : triton::arch::register_e::ID_REG_X86_ECX;
  auto XDX = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RDX : triton::arch::register_e::ID_REG_X86_EDX;
  auto XSP = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RSP : triton::arch::register_e::ID_REG_X86_ESP;
  auto XBP = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RBP : triton::arch::register_e::ID_REG_X86_EBP;
  auto XSI = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RSI : triton::arch::register_e::ID_REG_X86_ESI;
  auto XDI = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RDI : triton::arch::register_e::ID_REG_X86_EDI;
  auto XIP = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RIP : triton::arch::register_e::ID_REG_X86_EIP;
  // Save the registers
  mRegisters.insert(XAX);
  mRegisters.insert(XBX);
  mRegisters.insert(XCX);
  mRegisters.insert(XDX);
  mRegisters.insert(XSP);
  mRegisters.insert(XBP);
  mRegisters.insert(XSI);
  mRegisters.insert(XDI);
  mRegisters.insert(XIP);
  // Concretize the registers
  mApi->setConcreteRegisterValue(mApi->getRegister(XAX), mIs64Bit ? 0xDEADBEEF10000000 : 0xDEAD1000);
  mApi->setConcreteRegisterValue(mApi->getRegister(XBX), mIs64Bit ? 0xDEADBEEF20000000 : 0xDEAD2000);
  mApi->setConcreteRegisterValue(mApi->getRegister(XCX), mIs64Bit ? 0xDEADBEEF30000000 : 0xDEAD3000);
  mApi->setConcreteRegisterValue(mApi->getRegister(XDX), mIs64Bit ? 0xDEADBEEF40000000 : 0xDEAD4000);
  mApi->setConcreteRegisterValue(mApi->getRegister(XSI), mIs64Bit ? 0xDEADBEEF50000000 : 0xDEAD5000);
  mApi->setConcreteRegisterValue(mApi->getRegister(XDI), mIs64Bit ? 0xDEADBEEF60000000 : 0xDEAD6000);
  mApi->setConcreteRegisterValue(mApi->getRegister(XBP), mIs64Bit ? 0xDEADBEEFF0000000 : 0xDEADF000);
  mApi->setConcreteRegisterValue(mApi->getRegister(XSP), DEFAULT_STACK_BASE);
  mApi->setConcreteRegisterValue(mApi->getRegister(XIP), mIs64Bit ? XIPV : XIPV & 0xFFFFFFFF);
  if (mIs64Bit) {
    auto R8 = triton::arch::register_e::ID_REG_X86_R8;
    auto R9 = triton::arch::register_e::ID_REG_X86_R9;
    auto R10 = triton::arch::register_e::ID_REG_X86_R10;
    auto R11 = triton::arch::register_e::ID_REG_X86_R11;
    auto R12 = triton::arch::register_e::ID_REG_X86_R12;
    auto R13 = triton::arch::register_e::ID_REG_X86_R13;
    auto R14 = triton::arch::register_e::ID_REG_X86_R14;
    auto R15 = triton::arch::register_e::ID_REG_X86_R15;
    mApi->setConcreteRegisterValue(mApi->getRegister(R8),  0xDEADBEEF70000000);
    mApi->setConcreteRegisterValue(mApi->getRegister(R9),  0xDEADBEEF80000000);
    mApi->setConcreteRegisterValue(mApi->getRegister(R10), 0xDEADBEEF90000000);
    mApi->setConcreteRegisterValue(mApi->getRegister(R11), 0xDEADBEEFA0000000);
    mApi->setConcreteRegisterValue(mApi->getRegister(R12), 0xDEADBEEFB0000000);
    mApi->setConcreteRegisterValue(mApi->getRegister(R13), 0xDEADBEEFC0000000);
    mApi->setConcreteRegisterValue(mApi->getRegister(R14), 0xDEADBEEFD0000000);
    mApi->setConcreteRegisterValue(mApi->getRegister(R15), 0xDEADBEEFE0000000);
    mRegisters.insert(R8);
    mRegisters.insert(R9);
    mRegisters.insert(R10);
    mRegisters.insert(R11);
    mRegisters.insert(R12);
    mRegisters.insert(R13);
    mRegisters.insert(R14);
    mRegisters.insert(R15);
  }
  // Taint the registers
  mApi->taintRegister(mApi->getRegister(XAX));
  mApi->taintRegister(mApi->getRegister(XBX));
  mApi->taintRegister(mApi->getRegister(XCX));
  mApi->taintRegister(mApi->getRegister(XDX));
  mApi->taintRegister(mApi->getRegister(XSI));
  mApi->taintRegister(mApi->getRegister(XDI));
  mApi->taintRegister(mApi->getRegister(XBP));
  mApi->taintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_CF));
  mApi->taintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_PF));
  mApi->taintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_AF));
  mApi->taintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_ZF));
  mApi->taintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_SF));
  mApi->taintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_OF));
  if (mIs64Bit) {
    mApi->taintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R8));
    mApi->taintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R9));
    mApi->taintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R10));
    mApi->taintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R11));
    mApi->taintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R12));
    mApi->taintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R13));
    mApi->taintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R14));
    mApi->taintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R15));
  }
  // Taint the stack slots
  uint64_t slotsCount = mIs64Bit ? 19 : 11;
  uint64_t slotSize = mIs64Bit ? 8 : 4;
  for (size_t i = 0; i < slotsCount; i++)
    mApi->setTaintMemory(triton::arch::MemoryAccess(DEFAULT_STACK_BASE + i * slotSize, slotSize), true);
  // Take a fresh snapshot (with tainted registers and stack slots)
  mFreshState = std::make_unique<Snapshot>(mApi, mIs64Bit);
  mFreshState->takeSnapshot();
  // Save the binary sections
  for (const auto &section : mBinaryFile->sections())
    mSections[section.getAddress()] = section.getAddress() + section.getSize();
}

// Memory management

void Symbolizer::handleMemoryLoad(triton::arch::Instruction& instruction) const {
  for (auto &operand : instruction.operands) {
    if (operand.getType() == triton::arch::OP_MEM) {
      // Fetch the memory operand
      auto &memory = operand.getMemory();
      // Fetch the memory address components
      auto &base = memory.getConstBaseRegister();
      auto &index = memory.getConstIndexRegister();
      auto &scale = memory.getScale();
      auto &displacement = memory.getDisplacement();
      // Fetch the components values
      uint32_t size = memory.getSize();
      uint64_t baseValue = 0;
      uint64_t indexValue = 0;
      uint64_t scaleValue = scale.getValue();
      uint64_t displacementValue = displacement.getValue();
      if (base.getName() != "unknown") {
        if (base.getName() == "rip")
          baseValue += instruction.getSize();
        baseValue += mApi->getConcreteRegisterValue(base).convert_to<uint64_t>();
      }
      if (index.getName() != "unknown") {
        indexValue += mApi->getConcreteRegisterValue(index).convert_to<uint64_t>();
      }
      // Calculate the accessed address
      uint64_t address = (baseValue + indexValue * scaleValue + displacementValue) & (mIs64Bit ? 0xFFFFFFFFFFFFFFFF : 0xFFFFFFFF);
      // Fetch the address from the binary
      for (const auto &section : mSections) {
        if (address >= section.first && address < section.second) {
          llvm::ArrayRef<uint8_t> bytes;
          readBytes(address, size, bytes);
          mApi->setConcreteMemoryAreaValue(address, bytes.vec());
        }
      }
    }
  }
}

// Execution and processing functions

triton::arch::Instruction Symbolizer::disassemble(uint64_t Address) const {
  // Fetch the bytes from the binary
  llvm::ArrayRef<uint8_t> Bytes;
  auto Error = readBytes(Address, MAX_X64_INSTRUCTION_SIZE, Bytes);
  if (Error)
    llvm::report_fatal_error("Failed to read from the binary at " + llvm::to_string(Address));
  // Initialize the Triton instruction
  triton::arch::Instruction instruction;
  instruction.setOpcode(Bytes.data(), Bytes.size());
  instruction.setAddress(Address);
  // Disassemble the instruction
  mApi->disassembly(instruction);
  // Return the instruction
  return instruction;
}

StepType Symbolizer::step(uint64_t Address) {
  // Get the instruction's bytes at the current VA
  llvm::ArrayRef<uint8_t> Bytes;
  auto Error = readBytes(Address, MAX_X64_INSTRUCTION_SIZE, Bytes);
  if (Error)
    llvm::report_fatal_error("Failed to read from the binary at " + llvm::to_string(Address));
  // Call the standard step function
  return step(Bytes, Address);
}

StepType Symbolizer::step(llvm::ArrayRef<uint8_t>& Bytes, uint64_t Address) {
  // Initialize the Triton instruction
  std::shared_ptr<triton::arch::Instruction> TInstruction = std::make_shared<triton::arch::Instruction>();
  TInstruction->setOpcode(Bytes.data(), Bytes.size());
  TInstruction->setAddress(Address);
  // Disassembly the instruction (yes, I know...)
  mApi->disassembly(*TInstruction);
  // Detect junk taint spreading
  bool JunkSpread = handleJunkSpreadTainting(*TInstruction);
  // Handle the memory load
  handleMemoryLoad(*TInstruction);
  // Symbolically process the instruction
  if (!mApi->buildSemantics(*TInstruction)) {
    std::cout << "Failed to execute: " << TInstruction << std::endl;
    return StepType::STOP;
  }
  // Debug print the instruction
  if (mVerbose)
    std::cout << TInstruction << std::endl;
  // Save the current tainted input values
  handleInstructionTainting(*TInstruction, JunkSpread);
  // Save the latest processed instruction
  mLastInstruction = TInstruction;
  // Determine if we reached a split instruction
  if (TInstruction->isControlFlow())
    return StepType::SPLIT;
  // Notify we can continue with the exploration
  return StepType::CONTINUE;
}

std::shared_ptr<triton::arch::Instruction> Symbolizer::getLastInstruction() { return mLastInstruction; }

// Tainting handling (contains custom VMProtect logic)

void Symbolizer::populateJunkInstructions(const std::set<triton::uint32>& JunkInstructions) {
  // Setup the set of junk instructions to ignore when tainting
  mJunkInstructions = JunkInstructions;
}

bool Symbolizer::isJunkInstruction(const triton::arch::Instruction& TInstruction) const {
  // Check if the instruction is known to be junk in this protector
  bool IsJunk = mJunkInstructions.find(TInstruction.getType()) != mJunkInstructions.end();
  if (mVerbose && IsJunk) {
    std::cout << "[J] " << TInstruction << std::endl;
  }
  return IsJunk;
}

bool Symbolizer::handleJunkSpreadTainting(const triton::arch::Instruction& TInstruction) const {
  // Determine if it's a junk instruction
  if (isJunkInstruction(TInstruction))
    return false;
  // Detect the junk taint spreading
  switch (TInstruction.getType()) {
    case ID_INS_SHLD:
    case ID_INS_SHRD:
    case ID_INS_OR:
    case ID_INS_XOR:
    case ID_INS_SHL:
    case ID_INS_SHR:
    case ID_INS_SUB:
    case ID_INS_ADD:
    case ID_INS_AND:
    case ID_INS_MOV:
    case ID_INS_MOVZX: {
      auto Op0 = TInstruction.operands[0];
      auto Op1 = TInstruction.operands[1];
      if (Op0.getType() == triton::arch::operand_e::OP_REG) {
        if (Op1.getType() == triton::arch::operand_e::OP_REG) {
          auto Reg0 = Op0.getConstRegister();
          auto Reg1 = Op1.getConstRegister();
          if (!mApi->isRegisterTainted(Reg0) || !mApi->isRegisterTainted(Reg1)) {
            // Notify we found junk taint spreading
            return true;
          }
        }
      }
    } break;
    default: break;
  }
  return false;
}

void Symbolizer::handleInstructionTainting(triton::arch::Instruction& TInstruction, bool JunkSpreading) {
  // Untaint the registers victim of junk spreading
  if (JunkSpreading) {
    switch (TInstruction.getType()) {
      case ID_INS_SHLD:
      case ID_INS_SHRD:
      case ID_INS_OR:
      case ID_INS_XOR:
      case ID_INS_SHL:
      case ID_INS_SHR:
      case ID_INS_SUB:
      case ID_INS_ADD:
      case ID_INS_AND:
      case ID_INS_MOVZX: {
        auto Op0 = TInstruction.operands[0];
        auto Op1 = TInstruction.operands[1];
        if (Op0.getType() == triton::arch::operand_e::OP_REG) {
          if (Op1.getType() == triton::arch::operand_e::OP_REG) {
            auto Reg0 = Op0.getRegister();
            auto Reg1 = Op1.getRegister();
            // Untaint the destination
            mApi->untaintRegister(Reg0);
            // Mark the instruction as untainted
            TInstruction.setTaint(false);
          }
        }
      } break;
      case ID_INS_MOV: {
        auto Op0 = TInstruction.operands[0];
        auto Op1 = TInstruction.operands[1];
        if (Op0.getType() == triton::arch::operand_e::OP_REG) {
          if (Op1.getType() == triton::arch::operand_e::OP_REG) {
            auto Reg0 = Op0.getRegister();
            auto Reg1 = Op1.getRegister();
            auto MaxSize = mIs64Bit ? 8 : 4;
            if (Reg0.getSize() < MaxSize) {
              // Untaint the destination
              mApi->untaintRegister(Reg0);
              // Mark the instruction as untainted
              TInstruction.setTaint(false);
            }
          }
        }
      } break;
      default: break;
    }
  }
  // Show the current tainted values
  if (mVerbose) {
    std::cout << "[*] Load accesses" << std::endl;
    for (auto L : TInstruction.getLoadAccess()) {
      std::cout << "\t{ " << L.first << " -> T = " << mApi->isMemoryTainted(L.first) << ", S = " << mApi->isMemorySymbolized(L.first) << " }" << std::endl;
    }
    std::cout << "[*] Store accesses" << std::endl;
    for (auto S : TInstruction.getStoreAccess()) {
      std::cout << "\t{ " << S.first << " -> T = " << mApi->isMemoryTainted(S.first) << ", S = " << mApi->isMemorySymbolized(S.first) << " }" << std::endl;
    }
    std::cout << "[*] Read registers" << std::endl;
    for (auto R : TInstruction.getReadRegisters()) {
      std::cout << "\t{ " << R.first << " -> T = " << mApi->isRegisterTainted(R.first) << ", S = " << mApi->isRegisterSymbolized(R.first) << " }" << std::endl;
    }
    std::cout << "[*] Written registers" << std::endl;
    for (auto R : TInstruction.getWrittenRegisters()) {
      std::cout << "\t{ " << R.first << " -> T = " << mApi->isRegisterTainted(R.first) << ", S = " << mApi->isRegisterSymbolized(R.first) << " }" << std::endl;
    }
  }
  // If it's a known junk instruction
  if (isJunkInstruction(TInstruction)) {
    // Untaint the destinations
    for (const auto& S : TInstruction.getStoreAccess())
      mApi->untaintMemory(S.first);
    for (const auto& R : TInstruction.getWrittenRegisters())
      mApi->untaintRegister(R.first);
    // Untaint the instruction
    TInstruction.setTaint(false);
    // Exit
    return;
  }
  // Handle different instructions
  switch (TInstruction.getType()) {
    case ID_INS_PUSHFD:
    case ID_INS_PUSHFQ: {
      // Check if one of the EFLAGS bits is tainted
      bool IsEFLAGSTainted = false;
      std::set<triton::arch::register_e> EFLAGSBits{
        triton::arch::register_e::ID_REG_X86_CF, triton::arch::register_e::ID_REG_X86_PF,
        triton::arch::register_e::ID_REG_X86_AF, triton::arch::register_e::ID_REG_X86_ZF,
        triton::arch::register_e::ID_REG_X86_SF, triton::arch::register_e::ID_REG_X86_OF
      };
      for (auto EFLAGSBit : EFLAGSBits) {
        if (mApi->isRegisterTainted(mApi->getRegister(EFLAGSBit))) {
          IsEFLAGSTainted = true;
          break;
        }
      }
      // Mark the instruction as tainted if one of the bits is tainted
      if (IsEFLAGSTainted) {
        // Taint the instruction
        TInstruction.setTaint(true);
        // Untaint the registers after the push
        for (auto EFLAGSBit : EFLAGSBits) {
          mApi->untaintRegister(mApi->getRegister(EFLAGSBit));
        }
      }
    } break;
    case ID_INS_POPFD:
    case ID_INS_POPFQ: {
      // Check if the memory is tainted
      for (const auto& L : TInstruction.getLoadAccess()) {
        if (mApi->isMemoryTainted(L.first)) {
          // Untaint the memory
          mApi->untaintMemory(L.first);
        }
      }
    } break;
    case ID_INS_PUSH: {
      // Check if the pushed value is a register or immediate
      for (const auto& O : TInstruction.operands) {
        switch (O.getType()) {
          case triton::arch::operand_e::OP_REG: {
            // Get the register
            auto Register = O.getConstRegister();
            // Check if the register is tainted
            if (mApi->isRegisterTainted(Register)) {
              // Untaint the register after the push
              mApi->untaintRegister(Register);
            }
          } break;
          case triton::arch::operand_e::OP_IMM: {
            // Taint the stack slot
            for (const auto &S : TInstruction.getStoreAccess())
              mApi->taintMemory(S.first);
            // Taint the instruction
            TInstruction.setTaint(true);
          } break;
          default: break;
        }
      }
    } break;
    case ID_INS_POP: {
      // Check if the popped value is a register or memory
      for (const auto& O : TInstruction.operands) {
        switch (O.getType()) {
          case triton::arch::operand_e::OP_REG: {
            // Check if the memory is tainted
            for (const auto& L : TInstruction.getLoadAccess()) {
              if (mApi->isMemoryTainted(L.first)) {
                // Untaint the memory
                mApi->untaintMemory(L.first);
              }
            }
          } break;
          case triton::arch::operand_e::OP_MEM: {
            // Check if the memory is tainted
            for (const auto& L : TInstruction.getLoadAccess()) {
              if (mApi->isMemoryTainted(L.first)) {
                // Untaint the memory
                mApi->untaintMemory(L.first);
              }
            }
          } break;
          default: break;
        }
      }
    } break;
    case ID_INS_MOVZX:
    case ID_INS_MOV: {
      // Fetch the source and destination operands
      auto Destination = TInstruction.operands[0];
      auto Source = TInstruction.operands[1];
      // If it's MOV [MEM], REG -> untaint the register
      if (Destination.getType() == triton::arch::operand_e::OP_MEM) {
        if (Source.getType() == triton::arch::operand_e::OP_REG) {
          // Get the register and memory
          auto Memory = Destination.getConstMemory();
          auto Register = Source.getConstRegister();
          if (mApi->isRegisterTainted(Register)) {
            // Untaint the register
            mApi->untaintRegister(Register);
          } else {
            // Taint the memory
            mApi->taintMemory(Memory);
            // Taint the instruction
            TInstruction.setTaint(true);
          }
        }
      } else if (Destination.getType() == triton::arch::operand_e::OP_REG) {
        // If it's MOV REG, REG
        if (Source.getType() == triton::arch::operand_e::OP_REG) {
          if (TInstruction.getType() == ID_INS_MOV) {
            const auto &SourceRegister = Source.getConstRegister();
            if (SourceRegister.getId() != triton::arch::ID_REG_X86_RSP && SourceRegister.getId() != triton::arch::ID_REG_X86_ESP) {
              uint64_t SourceRegisterValue = mApi->getConcreteRegisterValue(SourceRegister).convert_to<uint64_t>();
              if (isStackAddress(SourceRegisterValue)) {
                // Taint the destination register
                mApi->setTaintRegister(Destination.getConstRegister(), true);
                // Taint the instruction
                TInstruction.setTaint(true);
              }
            }
          } else if (TInstruction.getType() == ID_INS_MOVZX) {
            // Untaint the instruction
            TInstruction.setTaint(false);
          }
        }
        // If it's MOV REG, [MEM]
        else if (Source.getType() == triton::arch::OP_MEM) {
          // Get the destination register and source memory
          auto Register = Destination.getConstRegister();
          auto Memory = Source.getConstMemory();
          // Get the memory base and index
          auto Base = Memory.getConstBaseRegister();
          auto Index = Memory.getConstIndexRegister();
          // Get the memory address and size
          auto MemAddress = Memory.getAddress();
          auto MemSize = Memory.getSize();
          // Check if we are writing to the stack pointer
          if (Register.getName() == "esp" || Register.getName() == "rsp") {
            // Untaint the stack register
            mApi->untaintRegister(Register);
          }
          // Check if we are reading from known stack memory (previously mapped by Triton and inside the stack range)
          if (mVerbose) {
            std::cout << "isConcreteMemoryValueDefined: " << mApi->isConcreteMemoryValueDefined(MemAddress, MemSize) << std::endl;
            std::cout << "isStackAddress: " << isStackAddress(MemAddress) << std::endl;
          }
          if (/*mApi->isConcreteMemoryValueDefined(MemAddress, MemSize) && */isStackAddress(MemAddress)) {
            // Taint the destination register
            mApi->taintRegister(Register);
            // Taint the instruction
            TInstruction.setTaint(true);
          }
        }
      }
    } break;
    case ID_INS_LEA: {
      // Fetch the source and destination operands
      auto Destination = TInstruction.operands[0];
      auto Source = TInstruction.operands[1];
      // If it's LEA REG, [MEM] -> taint the register
      if (Destination.getType() == triton::arch::OP_REG) {
        if (Source.getType() == triton::arch::OP_MEM) {
          // Get the register and memory
          auto Register = Destination.getConstRegister();
          auto Memory = Source.getConstMemory();
          auto Base = Memory.getConstBaseRegister();
          auto Index = Memory.getConstIndexRegister();
          if (Base.getName() == "unknown" && Index.getName() == "unknown") {
            // Taint the register
            mApi->taintRegister(Register);
            // Taint the instruction
            TInstruction.setTaint(true);
          } else if (Base.getName() == "rip") {
            // Taint the register
            mApi->taintRegister(Register);
            // Taint the instruction
            TInstruction.setTaint(true);
          }
        }
      }
    } break;
    case ID_INS_CALL: {
      // Taint the memory with the return address
      for (const auto &S : TInstruction.getStoreAccess())
        mApi->taintMemory(S.first);
      // Taint the instruction
      TInstruction.setTaint(true);
    } break;
    case ID_INS_XCHG: {
      // Fetch the source and destination operands
      auto Destination = TInstruction.operands[0];
      auto Source = TInstruction.operands[1];
      // If it's XCHG REG, REG -> untaint the instruction
      if (Destination.getType() == triton::arch::OP_REG && Source.getType() == triton::arch::OP_REG) {
        auto Reg1 = Destination.getConstRegister();
        auto Reg2 = Source.getConstRegister();
        if (Reg1 == Reg2) {
          // Untaint the instruction
          TInstruction.setTaint(false);
        } else if (Reg1.getSize() == 1 && Reg2.getSize() == 2) {
          // Untaint the instruction
          TInstruction.setTaint(false);
          // Untaint the registers
          mApi->untaintRegister(Reg1);
          mApi->untaintRegister(Reg2);
        }
      }
    } break;
    case ID_INS_RDTSC: {
      // Get the PC register
      auto PC = mApi->getRegister(mIs64Bit ? triton::arch::ID_REG_X86_RIP : triton::arch::ID_REG_X86_EIP);
      // Symbolize the output registers
      for (const auto& W : TInstruction.getWrittenRegisters()) {
        // Skip the PC register
        if (W.first == PC) continue;
        // Taint the register
        mApi->setTaintRegister(W.first, true);
      }
      // Taint the instruction
      TInstruction.setTaint(true);
    } break;
    case ID_INS_CPUID: {
      // Get the PC register
      auto PC = mApi->getRegister(mIs64Bit ? triton::arch::register_e::ID_REG_X86_RIP : triton::arch::register_e::ID_REG_X86_EIP);
      // Symbolize the output registers
      for (const auto& W : TInstruction.getWrittenRegisters()) {
        // Skip the PC register
        if (W.first == PC) continue;
        // Taint the register
        mApi->setTaintRegister(W.first, true);
      }
      // Taint the instruction
      TInstruction.setTaint(true);
    } break;
    default: break;
  }
}

void Symbolizer::untaintRegisters() {
  // Determine the proper registers
  auto XAX = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RAX : triton::arch::register_e::ID_REG_X86_EAX;
  auto XBX = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RBX : triton::arch::register_e::ID_REG_X86_EBX;
  auto XCX = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RCX : triton::arch::register_e::ID_REG_X86_ECX;
  auto XDX = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RDX : triton::arch::register_e::ID_REG_X86_EDX;
  auto XSP = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RSP : triton::arch::register_e::ID_REG_X86_ESP;
  auto XBP = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RBP : triton::arch::register_e::ID_REG_X86_EBP;
  auto XSI = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RSI : triton::arch::register_e::ID_REG_X86_ESI;
  auto XDI = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RDI : triton::arch::register_e::ID_REG_X86_EDI;
  // Untaint all the registers
  mApi->untaintRegister(mApi->getRegister(XAX));
  mApi->untaintRegister(mApi->getRegister(XBX));
  mApi->untaintRegister(mApi->getRegister(XCX));
  mApi->untaintRegister(mApi->getRegister(XDX));
  mApi->untaintRegister(mApi->getRegister(XSI));
  mApi->untaintRegister(mApi->getRegister(XDI));
  mApi->untaintRegister(mApi->getRegister(XBP));
  mApi->untaintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_CF));
  mApi->untaintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_PF));
  mApi->untaintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_AF));
  mApi->untaintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_ZF));
  mApi->untaintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_SF));
  mApi->untaintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_OF));
  // Handle the 64 bits untainting
  if (mIs64Bit) {
    mApi->untaintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R8));
    mApi->untaintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R9));
    mApi->untaintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R10));
    mApi->untaintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R11));
    mApi->untaintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R12));
    mApi->untaintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R13));
    mApi->untaintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R14));
    mApi->untaintRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R15));
  }
}

void Symbolizer::concretizeRegisters() {
  // Determine the proper registers
  auto XAX = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RAX : triton::arch::register_e::ID_REG_X86_EAX;
  auto XBX = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RBX : triton::arch::register_e::ID_REG_X86_EBX;
  auto XCX = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RCX : triton::arch::register_e::ID_REG_X86_ECX;
  auto XDX = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RDX : triton::arch::register_e::ID_REG_X86_EDX;
  auto XSP = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RSP : triton::arch::register_e::ID_REG_X86_ESP;
  auto XBP = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RBP : triton::arch::register_e::ID_REG_X86_EBP;
  auto XSI = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RSI : triton::arch::register_e::ID_REG_X86_ESI;
  auto XDI = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RDI : triton::arch::register_e::ID_REG_X86_EDI;
  // Untaint all the registers
  mApi->concretizeRegister(mApi->getRegister(XAX));
  mApi->concretizeRegister(mApi->getRegister(XBX));
  mApi->concretizeRegister(mApi->getRegister(XCX));
  mApi->concretizeRegister(mApi->getRegister(XDX));
  mApi->concretizeRegister(mApi->getRegister(XSI));
  mApi->concretizeRegister(mApi->getRegister(XDI));
  mApi->concretizeRegister(mApi->getRegister(XBP));
  mApi->concretizeRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_CF));
  mApi->concretizeRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_PF));
  mApi->concretizeRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_AF));
  mApi->concretizeRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_ZF));
  mApi->concretizeRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_SF));
  mApi->concretizeRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_OF));
  // Handle the 64 bits untainting
  if (mIs64Bit) {
    mApi->concretizeRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R8));
    mApi->concretizeRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R9));
    mApi->concretizeRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R10));
    mApi->concretizeRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R11));
    mApi->concretizeRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R12));
    mApi->concretizeRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R13));
    mApi->concretizeRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R14));
    mApi->concretizeRegister(mApi->getRegister(triton::arch::register_e::ID_REG_X86_R15));
  }
}

bool Symbolizer::isStackAddress(uint64_t Address) const {
  uint64_t LoStack = (DEFAULT_STACK_BASE - DEFAULT_STACK_SIZE);
  uint64_t HiStack = (DEFAULT_STACK_BASE + DEFAULT_STACK_SIZE);
  return (Address >= LoStack && Address < HiStack);
}

bool Symbolizer::isSectionAddress(uint64_t Address) const {
  for (const auto &section : mSections)
    if (Address >= section.first && Address < section.second)
      return true;
  return false;
}

// Snapshot handling

void Symbolizer::createSnapshot(uint64_t VA, uint64_t Source) {
  // Debug print the snapshot creation
  if (mVerbose)
    std::cout << "createSnapshot (Address = 0x" << std::hex << VA << ", Source = 0x" << std::hex << Source << ")" << std::endl;
  // Create the snapshot
  auto initSnapshot = [&]() {
    RegistersSnapshot RS;
    RS.Source = Source;
    for (const auto reg : mRegisters)
      RS.Context[reg] = mApi->getConcreteRegisterValue(mApi->getRegister(reg)).convert_to<uint64_t>();
    return RS;
  };
  // Save the snapshot
  auto Query = querySnapshot(VA, Source);
  switch (Query) {
    case SnapshotQuery::UNKNOWN: {
      mSnapshots[VA] = { initSnapshot() };
    } break;
    case SnapshotQuery::KNOWN: {
      mSnapshots[VA].push_back(initSnapshot());
    } break;
    case SnapshotQuery::EXISTS: {
      // We erase it as we already explored this connection
      for (uint64_t i = 0; i < mSnapshots[VA].size(); i++) {
        auto Snapshot = mSnapshots[VA][i];
        if (Snapshot.Source == Source) {
          mSnapshots[VA].erase(mSnapshots[VA].begin() + i);
          break;
        }
      }
      mSnapshots[VA].push_back(initSnapshot());
    } break;
    default: break;
  }
}

void Symbolizer::restoreSnapshot(uint64_t VA, uint64_t Source) {
  if (mVerbose)
    std::cout << "restoreSnapshot (Address = 0x" << std::hex << VA << ", Source = 0x" << std::hex << Source << ")" << std::endl;
  // Fetch the snapshots set
  auto SnapshotsSetIt = mSnapshots.find(VA);
  if (SnapshotsSetIt != mSnapshots.end()) {
    // Restore the proper snapshot
    for (auto& Snapshot : SnapshotsSetIt->second) {
      if (Snapshot.Source == Source) {
        // Restore the snapshot values
        mFreshState->restoreSnapshot();
        for (const auto &I : Snapshot.Context) {
          const auto reg = I.first;
          const auto val = I.second;
          mApi->setConcreteRegisterValue(mApi->getRegister(reg), val);
        }
        // Return after restore
        return;
      }
    }
  }
  // If we didn't find a valid snapshot, reset everything
  mFreshState->restoreSnapshot();
}

void Symbolizer::removeSnapshot(uint64_t VA, uint64_t Source) {
  if (mVerbose)
    std::cout << "removeSnapshot (Address = 0x" << std::hex << VA << ", Source = 0x" << std::hex << Source << ")" << std::endl;
  auto SnapshotsSetIt = mSnapshots.find(VA);
  if (SnapshotsSetIt != mSnapshots.end()) {
    for (uint64_t i = 0; i < SnapshotsSetIt->second.size(); i++) {
      auto Snapshot = SnapshotsSetIt->second[i];
      if (Snapshot.Source == Source) {
        SnapshotsSetIt->second.erase(SnapshotsSetIt->second.begin() + i);
        break;
      }
    }
  }
}

size_t Symbolizer::dumpSnapshots() const {
  // Get the amount of snapshots
  size_t Size = 0;
  for (const auto& IT : mSnapshots) {
    std::cout << "Destination: 0x" << std::hex << IT.first << std::endl;
    for (const auto& SS : IT.second) {
      std::cout << "> 0x" << std::hex << SS.Source << std::endl;
      Size++;
    }
  }
  // Return the amount of snapshots
  return Size;
}

SnapshotQuery Symbolizer::querySnapshot(uint64_t VA, uint64_t Source) {
  auto SnapshotsSetIt = mSnapshots.find(VA);
  if (SnapshotsSetIt != mSnapshots.end()) {
    for (const auto& Snapshot : SnapshotsSetIt->second) {
      if (Snapshot.Source == Source)
        return SnapshotQuery::EXISTS;
    }
    return SnapshotQuery::KNOWN;
  }
  return SnapshotQuery::UNKNOWN;
}

// File reading API

bool Symbolizer::is64Bit() const {
  if (const auto* PE = llvm::dyn_cast<llvm::object::COFFObjectFile>(mBinaryFile)) {
    return PE->is64();
  } else if (const auto* ELFObject = llvm::dyn_cast<llvm::object::ELF64LEObjectFile>(mBinaryFile)) {
    return true;
  } else if (const auto* ELFObject = llvm::dyn_cast<llvm::object::ELF32LEObjectFile>(mBinaryFile)) {
    return false;
  } else if (const auto* MachOObject = llvm::dyn_cast<llvm::object::MachOObjectFile>(mBinaryFile)) {
    return MachOObject->is64Bit();
  }
  report_fatal_error("is64Bit: Unsupported Object type (" + mBinaryFile->getFileFormatName() + ")\n");
}

uint64_t Symbolizer::getImageBase() const {
  if (const auto* PE = llvm::dyn_cast<llvm::object::COFFObjectFile>(mBinaryFile)) {
    return PE->getImageBase();
  } else if (const auto* ELFObject = llvm::dyn_cast<llvm::object::ELF64LEObjectFile>(mBinaryFile)) {
    auto Object = ELFObject->getELFFile();
    for (const auto& Phdr : unwrapOrErrorInternal(Object.program_headers())) {
      if (Phdr.p_type == llvm::ELF::PT_LOAD && Phdr.p_offset == 0) {
        return Phdr.p_vaddr;
      }
    }
    llvm::report_fatal_error("Loading address not found.\n");
  } else if (const auto* ELFObject = llvm::dyn_cast<llvm::object::ELF32LEObjectFile>(mBinaryFile)) {
    auto Object = ELFObject->getELFFile();
    for (const auto& Phdr : unwrapOrErrorInternal(Object.program_headers())) {
      if (Phdr.p_type == llvm::ELF::PT_LOAD && Phdr.p_offset == 0) {
        return Phdr.p_vaddr;
      }
    }
    llvm::report_fatal_error("Loading address not found.\n");
  } else if (const auto* MachOObject = llvm::dyn_cast<llvm::object::MachOObjectFile>(mBinaryFile)) {
    for (auto& LC : MachOObject->load_commands()) {
      if (this->mIs64Bit) {
        auto Segment = MachOObject->getSegment64LoadCommand(LC);
        auto SegName = std::string(Segment.segname);
        if (SegName == "__TEXT") {
          return Segment.vmaddr;
        }
      } else {
        auto Segment = MachOObject->getSegmentLoadCommand(LC);
        auto SegName = std::string(Segment.segname);
        if (SegName == "__TEXT") {
          return Segment.vmaddr;
        }
      }
    }
    llvm::report_fatal_error("Loading address not found.\n");
  }
  llvm::report_fatal_error("Unsupported Object type (" + mBinaryFile->getFileFormatName() + ")\n");
}

std::error_code Symbolizer::readBytes(uint64_t VA, uint64_t Size, llvm::ArrayRef<uint8_t>& Bytes) const {
  if (const auto* PE = llvm::dyn_cast<llvm::object::COFFObjectFile>(mBinaryFile)) {
    if (!PE->getRvaAndSizeAsBytes(VA - mImageBase, Size, Bytes))
      return std::error_code();
  } else if (const auto* ELFObject = llvm::dyn_cast<llvm::object::ELF64LEObjectFile>(mBinaryFile)) {
    auto Object = ELFObject->getELFFile();
    const uint8_t* BytesPointer = unwrapOrErrorInternal(Object.toMappedAddr(VA));
    Bytes = llvm::ArrayRef<uint8_t>(reinterpret_cast<const uint8_t*>(BytesPointer), Size);
    return std::error_code();
  } else if (const auto* ELFObject = llvm::dyn_cast<llvm::object::ELF32LEObjectFile>(mBinaryFile)) {
    auto Object = ELFObject->getELFFile();
    const uint8_t* BytesPointer = unwrapOrErrorInternal(Object.toMappedAddr(VA));
    Bytes = llvm::ArrayRef<uint8_t>(reinterpret_cast<const uint8_t*>(BytesPointer), Size);
    return std::error_code();
  } else if (const auto* MachOObject = llvm::dyn_cast<llvm::object::MachOObjectFile>(mBinaryFile)) {
    auto MemoryBuffer = MachOObject->getMemoryBufferRef();
    const char* BufferStart = MemoryBuffer.getBufferStart();
    const uint8_t* BytesPointer = (const uint8_t*)((uint64_t)BufferStart + VA - mImageBase);
    Bytes = llvm::ArrayRef<uint8_t>(reinterpret_cast<const uint8_t*>(BytesPointer), Size);
    return std::error_code();
  }
  llvm::report_fatal_error("readBytes: Unsupported Object type (" + mBinaryFile->getFileFormatName() + ")\n");
}

// PC interaction API

uint64_t Symbolizer::getNextAddress() const {
  auto XIP = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RIP : triton::arch::register_e::ID_REG_X86_EIP;
  return mApi->getConcreteRegisterValue(mApi->getRegister(XIP)).convert_to<uint64_t>();
}

void Symbolizer::setNextAddress(uint64_t PC) {
  auto XIP = mIs64Bit ? triton::arch::register_e::ID_REG_X86_RIP : triton::arch::register_e::ID_REG_X86_EIP;
  mApi->setConcreteRegisterValue(mApi->getRegister(XIP), PC);
}

void Symbolizer::resetStackPointer(const triton::arch::Register &vsp) {
  mApi->setConcreteRegisterValue(vsp, DEFAULT_STACK_BASE);
}

// Utility API

API *Symbolizer::getApi() { return mApi; }
