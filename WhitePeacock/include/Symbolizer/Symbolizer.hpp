#pragma once

// triton
#include <triton/x86Specifications.hpp>
#include <triton/api.hpp>

// llvm
#include <llvm/Support/raw_os_ostream.h>
#include <llvm/Object/MachOUniversal.h>
#include <llvm/Object/ELFObjectFile.h>
#include <llvm/Object/ObjectFile.h>
#include <llvm/Support/ErrorOr.h>
#include <llvm/Support/Error.h>
#include <llvm/Object/MachO.h>
#include <llvm/Object/COFF.h>

// snapshot
#include <Snapshot/Snapshot.hpp>

// Copied from LLVM
// https://github.com/llvm-mirror/llvm/blob/master/tools/llvm-readobj/llvm-readobj.h#L39
template <class T>
T unwrapOrErrorInternal(llvm::Expected<T> EO) {
  if (EO) return *EO;
  std::string Buf;
  llvm::raw_string_ostream OS(Buf);
  logAllUnhandledErrors(EO.takeError(), OS, "");
  OS.flush();
  llvm::report_fatal_error(Buf);
}

// defines
#define MAX_X64_INSTRUCTION_SIZE 15
#define DEFAULT_STACK_BASE 0x14E000
#define DEFAULT_STACK_SIZE 0x100000

typedef struct RegistersSnapshot {
  // Source TAG
  uint64_t Source;
  // Registers context
  std::map<triton::arch::register_e, uint64_t> Context;
} RegistersSnapshot;

// enumerations
enum SnapshotQuery {
  KNOWN,
  EXISTS,
  UNKNOWN
};

enum StepType {
  CONTINUE, // standard instruction, no side effects
  SPLIT,    // control flow instruction
  STOP      // unsupported instruction
};

class Symbolizer {
public:

  // Default constructor
  Symbolizer(const llvm::object::ObjectFile *BinaryFile, bool Verbose = false);

  // Default destructor
  ~Symbolizer();

  // Initialize the Triton context
  void setup(uint64_t RIP = 0x0);

  // Execute one step of the symbolization
  StepType step(llvm::ArrayRef<uint8_t>& Bytes, uint64_t Address);
  StepType step(uint64_t Address);

  // Disassemble an instruction
  triton::arch::Instruction disassemble(uint64_t Address) const;

  // Get the next CIP
  uint64_t getNextAddress() const;
  void setNextAddress(uint64_t PC);
  void resetStackPointer(const triton::arch::Register &vsp);

  // Get the Triton and LLVM API
  API *getApi();

  // Snapshot management

  size_t dumpSnapshots() const;

  SnapshotQuery querySnapshot(uint64_t VA, uint64_t Source);

  void restoreSnapshot(uint64_t VA, uint64_t Source);

  void createSnapshot(uint64_t VA, uint64_t Source);

  void removeSnapshot(uint64_t VA, uint64_t Source);

  // File interaction API

  std::error_code readBytes(uint64_t Address, uint64_t Size, llvm::ArrayRef<uint8_t>& Bytes) const;

  // Public taint management
  
  void populateJunkInstructions(const std::set<triton::uint32>& JunkInstructions);
  void untaintRegisters();

  // Public registers management

  void concretizeRegisters();

  // Public file management

  uint64_t getImageBase() const;
  bool is64Bit() const;

  bool isStackAddress(uint64_t Address) const;

  bool isSectionAddress(uint64_t Address) const;

  // Public instruction management

  std::shared_ptr<triton::arch::Instruction> getLastInstruction();

private:

  // LLVM variables
  API* mApi = nullptr;
  std::shared_ptr<triton::arch::Instruction> mLastInstruction;

  // Snapshots
  std::map<uint64_t, std::vector<RegistersSnapshot>> mSnapshots;
  std::set<triton::arch::register_e> mRegisters;

  // Fresh snapshot
  std::unique_ptr<Snapshot> mFreshState;

  // Taint engine structures
  std::set<triton::uint32> mJunkInstructions;

  // Binary sections
  std::map<uint64_t, uint64_t> mSections;

  // Flags
  bool mVerbose = false;

  // Mapped file information
  const llvm::object::ObjectFile *mBinaryFile;
  uint64_t mImageBase = 0;
  bool mIs64Bit = false;

  // Memory management
  void handleMemoryLoad(triton::arch::Instruction& instruction) const;

  // Taint engine API
  bool isJunkInstruction(const triton::arch::Instruction& Instruction) const;
  bool handleJunkSpreadTainting(const triton::arch::Instruction& inst) const;
  void handleInstructionTainting(triton::arch::Instruction& inst, bool JunkSpreading);
};