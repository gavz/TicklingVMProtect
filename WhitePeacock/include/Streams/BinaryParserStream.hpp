#pragma once

#include <map>

#include <llvm/Object/ObjectFile.h>

#include <Translator/CFGStructures.hpp>
#include <Streams/BasicBlockStream.hpp>
#include <Symbolizer/Symbolizer.hpp>

typedef struct VmJump {
  std::string mType;
  triton::arch::Register mIPRegister;
  triton::arch::Register mSPRegister;
  std::vector<uint64_t> mInstructions;
} VmJump;

class BinaryParserStream : BasicBlockStream {
private:

  // Unique pointer to the binary file
  llvm::Expected<llvm::object::OwningBinary<llvm::object::ObjectFile>> mObjectOrErr;
  std::unique_ptr<llvm::object::ObjectFile> mObject;

  // Symbolizer instance
  Symbolizer *mSymbolizer = nullptr;

  // Path to the opened file
  const std::string &mFilePath;

  // Maps a virtual address to a basic block
  std::map<uint64_t, whitepeacock::BasicBlock> mBasicBlocks;

  // Maps a virtual address to a real address
  std::map<uint64_t, uint64_t> mAddressMap;
  std::map<uint64_t, uint64_t> mVmNopsMap;

  // Sets containing the virtual stubs addresses
  std::set<uint64_t> mVmEnters;
  std::set<uint64_t> mVmStubs;

  // Jump, exit and nop instructions
  std::map<uint64_t, VmJump> mVmJumps;
  std::set<uint64_t> mVmExits;

  bool visitBasicBlock(uint64_t entryPoint, uint64_t source);

  void parseInputBinary();

  void setupSymbolizer();

  void collectVmEnters();

public:

  BinaryParserStream(const std::string &filePath);

  ~BinaryParserStream();

  std::string getImportName(uint64_t address) const;

  llvm::object::ObjectFile *getObjectFilePointer() const;

  void killSnapshot(uint64_t entryPoint) const;

  bool getBasicBlock(uint64_t entryPoint, whitepeacock::BasicBlock &basicBlock) override;

  bool setBasicBlockAddresses(uint64_t entryPoint, std::set<uint64_t> &destination);

  bool isUnsupportedInstructionExit(uint64_t address, uint64_t &stubAddress, std::vector<std::vector<uint8_t>> &opcodes) const;

  bool isPossibleVirtualStub(uint64_t address) const;

};
