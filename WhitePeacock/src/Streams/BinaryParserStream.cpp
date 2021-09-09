#include <llvm/Support/CommandLine.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Object/MachOUniversal.h>
#include <llvm/Object/ObjectFile.h>
#include <llvm/Object/MachO.h>
#include <llvm/Object/COFF.h>

#include <Streams/BinaryParserStream.hpp>
#include <Translator/SVMTranslator.hpp>
#include <Symbolizer/Symbolizer.hpp>

#include <sstream>

llvm::cl::opt<bool> VerboseSymbolizer("v",
  llvm::cl::desc("Enable the verbose mode on the Symbolizer"),
  llvm::cl::value_desc("debug"),
  llvm::cl::Optional);

llvm::cl::opt<std::string> Sections("s",
  llvm::cl::desc("Comma separated list of sections names to parse"),
  llvm::cl::value_desc("sections"),
  llvm::cl::Optional);

BinaryParserStream::~BinaryParserStream() { delete mSymbolizer; }

BinaryParserStream::BinaryParserStream(const std::string &filePath) : 
mObjectOrErr(llvm::object::ObjectFile::createObjectFile(filePath)),
mFilePath(filePath)
{
  parseInputBinary();
  setupSymbolizer();
  collectVmEnters();
}

void BinaryParserStream::parseInputBinary() {
  // Create an object file
  if (auto err = mObjectOrErr.takeError())
    llvm::report_fatal_error("Error opening: " + mFilePath);

  // Create a binary file
  llvm::Expected<llvm::object::ObjectFile *> mBinaryOrErr = mObjectOrErr->getBinary();
  if (auto err = mBinaryOrErr.takeError())
    llvm::report_fatal_error("Error while parsing the file: " + mFilePath);

  // Parse the binary object
  auto Binary = mBinaryOrErr.get();
  if (Binary->isCOFF()) {
    llvm::outs() << "[*] COFF binary detected\n";
    auto UniqPtrPEObjectOrError = Binary->createCOFFObjectFile(Binary->getMemoryBufferRef());
    if (auto err = UniqPtrPEObjectOrError.takeError())
      llvm::report_fatal_error("Error while parsing the file PE object.");
    mObject = std::move(*UniqPtrPEObjectOrError);
  } else if (Binary->isELF()) {
    llvm::outs() << "[*] ELF binary detected\n";
    auto UniqPtrELFObjectOrError = Binary->createELFObjectFile(Binary->getMemoryBufferRef());
    if (auto err = UniqPtrELFObjectOrError.takeError())
      llvm::report_fatal_error("Error while parsing the file ELF object.");
    mObject = std::move(*UniqPtrELFObjectOrError);
  } else if (Binary->isMachO()) {
    llvm::outs() << "[*] MachO binary detected\n";
    auto UniqPtrMachOObjectOrError = Binary->createMachOObjectFile(Binary->getMemoryBufferRef());
    if (auto err = UniqPtrMachOObjectOrError.takeError())
      llvm::report_fatal_error("Error while parsing the file MachO object.");
    mObject = std::move(*UniqPtrMachOObjectOrError);
  } else if (Binary->isMachOUniversalBinary()) {
    llvm::report_fatal_error("MachO (Universal) binary detected: UNSUPPORTED!");
  } else {
    llvm::report_fatal_error("Unsupported binary format.");
  }
}

void BinaryParserStream::setupSymbolizer() {
  // Allocate the symbolizer instance
  mSymbolizer = new Symbolizer(mObject.get(), VerboseSymbolizer);
  // Setup the initial concrete registers
  mSymbolizer->setup();
  // Setup the ad-hoc junk instructions
  mSymbolizer->populateJunkInstructions({
    ID_INS_CMOVA, ID_INS_CMOVAE, ID_INS_CMOVB, ID_INS_CMOVBE, ID_INS_CMOVE, ID_INS_CMOVG,
    ID_INS_CMOVGE, ID_INS_CMOVL, ID_INS_CMOVLE, ID_INS_CMOVNE, ID_INS_CMOVNO, ID_INS_CMOVNP,
    ID_INS_CMOVNS, ID_INS_CMOVO, ID_INS_CMOVP, ID_INS_CMOVS, ID_INS_BT, ID_INS_BTC, ID_INS_BTR,
    ID_INS_BTS, ID_INS_CBW, ID_INS_CWD, ID_INS_CDQ, ID_INS_CDQE, ID_INS_CQO, ID_INS_MOVSX,
    ID_INS_MOVSXD, ID_INS_RCL, ID_INS_RCR, ID_INS_ROL, ID_INS_ROR, ID_INS_INC, ID_INS_DEC, ID_INS_BSWAP,
    ID_INS_TEST, ID_INS_CMP, ID_INS_SETAE, ID_INS_SETA, ID_INS_SETBE, ID_INS_SETB,
    ID_INS_SETE, ID_INS_SETGE, ID_INS_SETG, ID_INS_SETLE, ID_INS_SETL, ID_INS_SETNE, ID_INS_SETNO,
    ID_INS_SETNP, ID_INS_SETNS, ID_INS_SETO, ID_INS_SETP, ID_INS_SETS, ID_INS_SBB, ID_INS_SAR,
    ID_INS_SAL, ID_INS_LAHF, ID_INS_ADC, ID_INS_BSF, ID_INS_BSR, ID_INS_CMC, ID_INS_STC, ID_INS_STD,
    ID_INS_CLC, ID_INS_NEG, ID_INS_CWDE, ID_INS_XADD, ID_INS_ARPL
  });
}

void BinaryParserStream::collectVmEnters() {
  #define CALL_JMP_SIZE 5
  // Fetch a section given an address
  auto getSectionByAddress = [&](uint64_t address) -> llvm::Optional<llvm::object::SectionRef> {
    for (const auto &section : mObject->sections()) {
      uint64_t lo = section.getAddress();
      uint64_t hi = lo + section.getSize();
      if (address >= lo && address < hi)
        return llvm::Optional(section);
    }
    return llvm::Optional<llvm::object::SectionRef>();
  };
  // Parse the user-specified sections
  std::set<std::string> sections;
  std::stringstream SS(Sections);
  std::string name;
  while (std::getline(SS, name, ','))
    sections.insert(name);
  // Iterate all the sections in the binary
  for (const auto &section : mObject->sections()) {
    // Skip if non executable
    if (!section.isText())
      continue;
    // Print the section's name
    auto sectionNameOrErr = section.getName();
    if (sectionNameOrErr.takeError()) {
      llvm::errs() << "[!] Unable to get the section's name.\n";
      continue;
    }
    const auto &secName = sectionNameOrErr.get();
    // Determine if we need to parse this section
    if (!sections.empty() && !sections.count(secName.str()))
      continue;
    llvm::outs() << "[i] Parsing: " << secName << "\n";
    // Fetch the section's content
    auto sectionDataOrErr = section.getContents();
    if (sectionDataOrErr.takeError()) {
      llvm::errs() << "[!] Unable to get the section's data.\n";
      continue;
    }
    const auto &secData = sectionDataOrErr.get();
    const unsigned char *secBytes = secData.bytes_begin();
    for (size_t i = 0; i < (secData.size() - CALL_JMP_SIZE); i++) {
      // Identify a jump or call to a virtual routine/stub
      if (secBytes[i] != 0xE9 && secBytes[i] != 0xE8)
        continue;
      // Determine the address of the instruction and destination
      uint64_t insnAddress = section.getAddress() + i;
      uint64_t jumpAddress = insnAddress + CALL_JMP_SIZE + *(int32_t *)&secBytes[i + 1];
      // Fetch the destination section
      const auto &dstSection = getSectionByAddress(jumpAddress);
      if (!dstSection.hasValue() || !dstSection->isText())
        continue;
      // Identify if it's a virtual routine or stub
      if (dstSection->getAddress() == section.getAddress()) {
        // Check if it's a virtual stub pattern
        if (i >= CALL_JMP_SIZE && secBytes[i - CALL_JMP_SIZE] == 0x68) {
          // Determine the offset of the first stub instruction
          uint64_t stubOffset = jumpAddress - section.getAddress();
          // Check if it's a virtual stub pattern
          if ((stubOffset + 1) <= secData.size()) {
            unsigned char byte0 = secBytes[stubOffset];
            unsigned char byte1 = secBytes[stubOffset + 1];
            if ((byte0 >= 0x50 && byte0 <= 0x57) || (byte1 >= 0x50 && byte1 <= 0x57 && byte0 == 0x41))
              mVmStubs.insert(insnAddress - CALL_JMP_SIZE);
            else if (byte0 == 0x9C)
              mVmStubs.insert(insnAddress - CALL_JMP_SIZE);
          }
        }
        // Check if it's a virtual routine pattern
        else {
          auto dstSectionDataOrErr = dstSection->getContents();
          if (dstSectionDataOrErr.takeError())
            continue;
          const auto &dstSecData = dstSectionDataOrErr.get();
          const unsigned char *dstSecBytes = dstSecData.bytes_begin();
          // Determine the offset of the jump instruction
          uint64_t jmpOffset = jumpAddress - dstSection->getAddress();
          // Check if it's a virtual stub pattern
          if (dstSecBytes[jmpOffset] == 0x68 && dstSecBytes[jmpOffset + CALL_JMP_SIZE] == 0xE8)
            mVmEnters.insert(insnAddress);
        }
      }
      // Check if it's a virtual routine
      else {
        // Fetch the destination section's content
        auto dstSectionDataOrErr = dstSection->getContents();
        if (dstSectionDataOrErr.takeError())
          continue;
        const auto &dstSecData = dstSectionDataOrErr.get();
        const unsigned char *dstSecBytes = dstSecData.bytes_begin();
        // Determine the offset of the jump instruction
        uint64_t jmpOffset = jumpAddress - dstSection->getAddress();
        // Check if it's a virtual stub pattern
        if (dstSecBytes[jmpOffset] == 0x68 && dstSecBytes[jmpOffset + CALL_JMP_SIZE] == 0xE8)
          mVmEnters.insert(insnAddress);
      }
    }
  }
  // Print the discovered VmEnters
  if (!mVmEnters.empty()) {
    llvm::outs() << "[*] Discovered VmEnter(s):\n";
    for (uint64_t vmEnter : mVmEnters)
      std::cout << "  - 0x" << std::hex << vmEnter << std::endl;
  }
  // Create a pristine snapshot for each VmEnter
  for (uint64_t vmEnter : mVmEnters) {
    mSymbolizer->setNextAddress(vmEnter);
    mSymbolizer->createSnapshot(vmEnter, vmEnter);
  }
}

bool BinaryParserStream::visitBasicBlock(uint64_t entryPoint, uint64_t source) {
  // Restore the symbolic snapshot
  if (mSymbolizer->querySnapshot(entryPoint, source) == SnapshotQuery::EXISTS)
    mSymbolizer->restoreSnapshot(entryPoint, source);
  else
    mSymbolizer->restoreSnapshot(1, 1);
  // Delete the symbolic snapshot
  mSymbolizer->removeSnapshot(entryPoint, source);
  // Track the virtual structures
  whitepeacock::BasicBlock basicBlock;
  auto vmBlock = std::make_shared<VirtualBasicBlock>();
  auto vmInstruction = std::make_shared<VirtualInstruction>();
  // Lambda function to parse an hex string
  auto parseHex = [](const std::string &str, uint64_t &value) {
    if (str.length() < 3 || str.substr(0, 2) != "0x")
      return false;
    std::stringstream ss(str);
    return (bool)(ss >> std::hex >> value);
  };
  // Lambda function to save a parsed virtual instruction
  auto saveParsedVirtualInstruction = [&]() {
    for (const auto &str : vmInstruction->mVmBody) {
      std::stringstream ss(str);
      whitepeacock::Instruction instruction;
      if (std::getline(ss, instruction.Mnemonic, ' ')) {
        std::string operandStr;
        while (std::getline(ss, operandStr, ' ')) {
          auto &operand = instruction.Operands.emplace_back();
          if (operandStr.substr(0, 2) == "0x") { // Immediate
            operand.Type = whitepeacock::OperandType::Imm;
            if (!parseHex(operandStr, operand.Imm))
              llvm::report_fatal_error("Failed to parse an immediate value!");
          } else { // Variable
            operand.Type = whitepeacock::OperandType::Var;
            operand.Var = std::move(operandStr);
          }
        }
      }
      basicBlock.Instructions.push_back(std::move(instruction));
    }
  };
  // Lambda function to save a virtual instruction
  auto saveVirtualInstruction = [&]() {
    // 1. Match the virtual instruction
    if (!IdentifyHandler(mSymbolizer, vmInstruction, mSymbolizer->is64Bit()))
      llvm::report_fatal_error("Failed to match a virtual instruction!");
    // 2. Save the virtual instruction
    vmBlock->saveInstruction(vmInstruction);
    // 3. Parse the virtual instruction
    saveParsedVirtualInstruction();
    // 4. Reset the stack pointer if needed
    if (vmInstruction->mType == HandlerType::POP_REG_SP) {
      const auto &first = vmInstruction->mTaintedBody[0];
      const auto &vsp = first->mInstruction->operands[0].getConstRegister();
      mSymbolizer->resetStackPointer(vsp);
    }
    // 5. Save the virtual jump or exit
    if (vmInstruction->mType == HandlerType::VM_JUMP) {
      const auto &jumpBody = vmInstruction->mUntaintedBody;
      VmJump vmJump;
      vmJump.mType = vmInstruction->mVmBody[0];
      vmJump.mIPRegister = jumpBody[0]->mInstruction->operands[0].getConstRegister();
      vmJump.mSPRegister = jumpBody[0]->mInstruction->operands[1].getConstMemory().getConstBaseRegister();
      for (size_t i = 1; i < jumpBody.size(); i++)
        vmJump.mInstructions.push_back(jumpBody[i]->mInstruction->getAddress());
      mVmJumps[source] = vmJump;
    } else if (vmInstruction->mType == HandlerType::VM_EXIT)
      mVmExits.insert(source);
    else if (vmInstruction->mType == HandlerType::VM_NOP)
      mVmNopsMap[source] = mSymbolizer->getNextAddress();
    // 6. Reset the virtual instruction
    vmInstruction->reset();
    // 7. Untaint the registers
    mSymbolizer->untaintRegisters();
  };
  // Lambda function to save a native instruction
  auto saveNativeInstruction = [&]() {
    auto *api = mSymbolizer->getApi();
    // 1. Fetch the latest instruction
    const auto &instruction = mSymbolizer->getLastInstruction();
    // 2. Populate a native instruction
    auto nativeInstruction = std::make_shared<NativeInstruction>(instruction);
    // 3. Save the values of the written registers
    for (const auto &R : instruction->getWrittenRegisters()) {
      // Ignore the flag registers
      if (api->isFlag(R.first))
        continue;
      // Ignore the PC register
      if (R.first.getName() == "rip" || R.first.getName() == "eip")
        continue;
      // Save the value
      nativeInstruction->mWrittenRegisters[R.first] = R.second->evaluate().convert_to<uint64_t>();
    }
    // 4. Save the values of the read registers
    for (const auto &R : instruction->getReadRegisters()) {
      // Ignore the flag registers
      if (api->isFlag(R.first)) continue;
      // Save the value
      nativeInstruction->mReadRegisters[R.first] = R.second->evaluate().convert_to<uint64_t>();
    }
    // 5. Save the handler instruction
    vmInstruction->saveUntaintedInstruction(nativeInstruction);
    // 6. Save the tainted handler instruction
    if (instruction->isTainted()) {
      // std::cout << "> " << nativeInstruction->mInstruction << std::endl;
      vmInstruction->saveTaintedInstruction(nativeInstruction);
    }
  };
  // Lambda function to use slots in the prologue/epilogue
  auto adjustPrologueEpilogue = [&](whitepeacock::BasicBlock &BB) {
    // Identify the block type
    const auto &first = BB.Instructions.front();
    const auto &last = BB.Instructions.back();
    auto &insns = BB.Instructions;
    bool IsVmEnter = first.Mnemonic.rfind("PUSH_IMM", 0) == 0;
    bool IsVmExit = last.Mnemonic == "EXIT";
    // Convert the prologue
    if (!IsVmEnter) {
      size_t prologueSize = 19;
      for (size_t i = 0; i < prologueSize; i++) {
        auto Op = whitepeacock::Operand("slots[" + std::to_string(i) + "]");
        BB.Instructions[i].Mnemonic = "MOVE_VMREG_SLOT";
        BB.Instructions[i].Operands.push_back(Op);
      }
    }
    // Convert the epilogue
    if (!IsVmExit) {
      size_t epilogueStart = BB.Instructions.size() - 2;
      size_t epilogueSize = 19;
      for (size_t i = 0; i < epilogueSize; i++) {
        whitepeacock::Instruction move("POP_SLOT", "slots[" + std::to_string(i) + "]");
        BB.Instructions.insert(BB.Instructions.begin() + epilogueStart + i, move);
      }
    }
  };
  // Initialize the basic block
  basicBlock.EntryPoint = source;
  // Execute the instructions till a virtual JUMP or EXIT
  bool continueExecution = true;
  uint64_t PC = entryPoint;
  while (continueExecution) {
    switch (mSymbolizer->step(PC)) {
      case StepType::CONTINUE: {
        // Handling a plain instruction
        saveNativeInstruction();
      } break;
      case StepType::SPLIT: {
        // Save the native instruction
        saveNativeInstruction();
        // Reach an handler stop instruction
        const auto& instruction = mSymbolizer->getLastInstruction();
        switch (instruction->getType()) {
          case ID_INS_RET: {
            saveVirtualInstruction();
          } break;
          case ID_INS_JMP: {
            if (instruction->operands[0].getType() == triton::arch::operand_e::OP_REG)
              saveVirtualInstruction();
          } break;
          case ID_INS_CALL: {
            const auto &Op0 = instruction->operands[0];
            if (instruction->isTainted() && Op0.getType() == triton::arch::OP_REG)
              llvm::report_fatal_error("Found VM_CALL, implement the proper handling!");
          } break;
          default: break;
        }
        // Stop if we reached a VM_EXIT, VM_JUMP or VM_NOP
        if (vmInstruction->mType == HandlerType::VM_JUMP ||
            vmInstruction->mType == HandlerType::VM_EXIT ||
            vmInstruction->mType == HandlerType::VM_NOP)
          continueExecution = false;
      } break;
      case StepType::STOP: {
        std::stringstream ss;
        ss << __func__;
        ss << ": unsupported Triton instruction!\n";
        llvm::report_fatal_error(ss.str());
      } break;
    }
    // Fetch the next PC
    PC = mSymbolizer->getNextAddress();
  }
  // Use the slots in the basic blocks prologue/epilogue
  adjustPrologueEpilogue(basicBlock);
  // Identify if it's an exit block
  if (basicBlock.Instructions.back().Mnemonic == "EXIT")
    basicBlock.BlockTy = whitepeacock::BlockType::Unconditional;
  // Store the basic block in the map
  mBasicBlocks[source] = basicBlock;
  // Return success
  return true;
}

bool BinaryParserStream::getBasicBlock(uint64_t entryPoint, whitepeacock::BasicBlock &basicBlock) {
  // If the entry point is known, return the basic block
  auto bbItr = mBasicBlocks.find(entryPoint);
  if (bbItr != mBasicBlocks.end()) {
    basicBlock = bbItr->second;
    return true;
  }
  // If the entry point is a VmEnter or VmStub address, explore it
  if (mVmEnters.count(entryPoint) || mVmStubs.count(entryPoint)) {
    if (!visitBasicBlock(entryPoint, entryPoint)) {
      std::stringstream ss;
      ss << __func__;
      ss << ": failed to explore the virtual stub @ 0x";
      ss << std::hex << entryPoint << "\n";
      llvm::report_fatal_error(ss.str());
    }
  }
  // If the basic block needs to be explored, do it
  auto addrItr = mAddressMap.find(entryPoint);
  if (addrItr != mAddressMap.end()) {
    if (!visitBasicBlock(addrItr->second, entryPoint)) {
      std::stringstream ss;
      ss << __func__;
      ss << ": failed to explore the basic block @ 0x";
      ss << std::hex << entryPoint << "\n";
      llvm::report_fatal_error(ss.str());
    }
  }
  // Try to fetch again the basic block
  auto bbItr2 = mBasicBlocks.find(entryPoint);
  if (bbItr2 != mBasicBlocks.end()) {
    basicBlock = bbItr2->second;
    return true;
  }
  // Otherwise fail
  return false;
}

bool BinaryParserStream::setBasicBlockAddresses(uint64_t entryPoint, std::set<uint64_t> &destinations) {
  // Verify if it's a VmExit
  if (mVmExits.count(entryPoint)) {
    std::cout << "VmExit: 0x" << std::hex << entryPoint << std::endl;
    return false;
  }
  // Verify if it's a VmNop
  if (mVmNopsMap.count(entryPoint)) {
    std::cout << "VmNop: 0x" << std::hex << entryPoint << std::endl;
    // Sanity check
    if (destinations.size() != 1) {
      std::stringstream ss;
      ss << __func__;
      ss << ": expected a single destination for a VM_NOP!";
      llvm::report_fatal_error(ss.str());
    }
    // Extract the destination and create a snapshot
    for (auto destination : destinations) {
      mAddressMap[destination] = mVmNopsMap[entryPoint];
      mSymbolizer->createSnapshot(mAddressMap[destination], destination);
    }
    return true;
  }
  // Verify if it's a VmJump
  if (!mVmJumps.count(entryPoint)) {
    std::stringstream ss;
    ss << __func__;
    ss << ": missing entry point ";
    ss << "(0x" << std::hex << entryPoint << ")";
    ss << std::endl;
    llvm::report_fatal_error(ss.str());
  }
  // Fetch the virtual jump for this basic block
  const auto &vmJump = mVmJumps[entryPoint];
  // Obtain the pointer to the Triton API
  auto *api = mSymbolizer->getApi();
  // Map the virtual destinations to binary destinations
  std::set<uint64_t> wrongDestinations;
  for (auto destination : destinations) {
    // Restore a fresh snapshot
    mSymbolizer->restoreSnapshot(0, 0);
    // Adjust the address to point to the encoded virtual basic block address
    uint64_t address = (vmJump.mType == "JUMP_DEC") ? (destination + 4) : (destination - 4);
    // Concretize the address register
    api->setConcreteRegisterValue(vmJump.mIPRegister, address);
    // Concretize the stack register
    api->setConcreteRegisterValue(vmJump.mSPRegister, DEFAULT_STACK_BASE);
    // Emulate the untainted instructions
    for (size_t i = 1; i < vmJump.mInstructions.size(); i++)
      mSymbolizer->step(vmJump.mInstructions[i]);
    // Read the next address
    mAddressMap[destination] = mSymbolizer->getNextAddress();
    // Taint the stack slots
    uint64_t stackPointer = DEFAULT_STACK_BASE;
    uint64_t slotsCount = mSymbolizer->is64Bit() ? 19 : 11;
    uint64_t slotSize = mSymbolizer->is64Bit() ? 8 : 4;
    for (size_t i = 0; i < slotsCount; i++)
      api->setTaintMemory(triton::arch::MemoryAccess(stackPointer + i * slotSize, slotSize), true);
    // Untaint the registers
    mSymbolizer->untaintRegisters();
    // Take a snapshot
    if (mSymbolizer->isSectionAddress(mAddressMap[destination]) && mSymbolizer->isSectionAddress(destination))
      mSymbolizer->createSnapshot(mAddressMap[destination], destination);
    else
      wrongDestinations.insert(destination);
  }
  // Kill the wrong destinations
  for (uint64_t destination : wrongDestinations)
    destinations.erase(destination);
  return true;
}

void BinaryParserStream::killSnapshot(uint64_t address) const {
  auto addrItr = mAddressMap.find(address);
  if (addrItr != mAddressMap.end())
    mSymbolizer->removeSnapshot(addrItr->second, address);
}

bool BinaryParserStream::isPossibleVirtualStub(uint64_t address) const {
  return mVmStubs.count(address);
}

bool BinaryParserStream::isUnsupportedInstructionExit(uint64_t address, uint64_t &stubAddress, std::vector<std::vector<uint8_t>> &opcodes) const {
  // Skip the failure address
  if (address == 0)
    return false;
  do {
    // Disassemble the current instruction
    const auto &instruction = mSymbolizer->disassemble(address);
    // Determine if it's a VmStub instruction
    if (mVmStubs.count(address)) {
      stubAddress = address;
      return true;
    }
    // Bail out with a failure
    if (instruction.isControlFlow())
      return false;
    // Save the opcode of the instruction
    auto *bytes = instruction.getOpcode();
    std::vector<uint8_t> opcode;
    for (size_t i = 0; i < instruction.getSize(); i++)
      opcode.push_back(bytes[i]);
    opcodes.push_back(opcode);
    // Get the address of the next instruction
    address += instruction.getSize();
  } while(true);
}

llvm::object::ObjectFile *BinaryParserStream::getObjectFilePointer() const {
  return mObject.get();
}

std::string BinaryParserStream::getImportName(uint64_t address) const {
  // Detect the address size
  size_t addressSize = mSymbolizer->is64Bit() ? 8 : 4;
  // Loop the import table to find the address
  if (const auto *PE = llvm::dyn_cast<llvm::object::COFFObjectFile>(mObject.get())) {
    for (const auto &sharedModule : PE->import_directories()) {
      // Get the RVA of the imported shared module
      uint32_t importAddressTableRVA;
      if (sharedModule.getImportAddressTableRVA(importAddressTableRVA))
        continue;
      // Symbols counter
      size_t symbolsCount = 0;
      for (const auto &importedSymbol : sharedModule.imported_symbols()) {
        // Determine the symbol address
        uint64_t symbolAddress = (addressSize * symbolsCount) + importAddressTableRVA + PE->getImageBase();
        // Check if it's the symbol we are looking for
        if (symbolAddress == address) {
          llvm::StringRef symbolName;
          llvm::StringRef moduleName;
          if (sharedModule.getName(moduleName))
            return "<missing module name>";
          if (importedSymbol.getSymbolName(symbolName))
            return "<missing symbol name>";
          return (moduleName.str() + "::" + symbolName.str());
        }
        // Go to the next symbol
        symbolsCount++;
      }
    }
  } else if (const auto *ELFObject = llvm::dyn_cast<llvm::object::ELF32LEObjectFile>(mObject.get())) {
    for (const auto &symbol : ELFObject->symbols()) {
      auto addressOrErr = symbol.getAddress();
      if (auto err = addressOrErr.takeError())
        continue;
      uint64_t symbolAddress = addressOrErr.get();
      if (symbolAddress == address) {
        auto nameOrErr = symbol.getName();
        if (auto err = nameOrErr.takeError())
          continue;
        auto symbolName = nameOrErr.get();
        return symbolName.str();
      }
    }
  } else if (const auto *ELFObject = llvm::dyn_cast<llvm::object::ELF64LEObjectFile>(mObject.get())) {
    for (const auto &symbol : ELFObject->symbols()) {
      auto addressOrErr = symbol.getAddress();
      if (auto err = addressOrErr.takeError())
        continue;
      uint64_t symbolAddress = addressOrErr.get();
      if (symbolAddress == address) {
        auto nameOrErr = symbol.getName();
        if (auto err = nameOrErr.takeError())
          continue;
        auto symbolName = nameOrErr.get();
        return symbolName.str();
      }
    }
  } else {
    // TODO: add support for Mach-O
    report_fatal_error("Unsupported Object type (" + mObject->getFileFormatName() + ")\n");
  }
  // No import found, return an empty string
  return "<not an import>";
}
