#include <Streams/BinaryParserStream.hpp>
#include <Lifter/Lifter.hpp>

#include <souper/Inst/Inst.h>

#include <queue>
#include <set>

using UnOps = std::vector<std::vector<uint8_t>>;

typedef struct VirtualStub {
  llvm::Function *CFG;
  llvm::Function *splittedCFG;
  uint64_t EntryPoint;
  std::set<uint64_t> Destinations;
  std::set<const llvm::Value *> Calls;
  std::set<const llvm::Value *> Children;
  std::set<const llvm::Value *> Unsupported;
  std::map<uint64_t, uint64_t> UnsupportedMap;
  std::map<uint64_t, UnOps> UnsupportedOpcodes;
} VirtualStub;

typedef struct ProvedConstant {
  souper::Inst *I;
  uint64_t V;

  ProvedConstant(souper::Inst *I, uint64_t V) : I(I), V(V) {}
} ProvedConstant;

class Explorer {
public:

  Explorer(uint64_t address, Lifter &lifter, BinaryParserStream &stream);

  bool run();

private:

  BinaryParserStream &mStream;
  Lifter &mLifter;
  uint64_t mAddress;

  std::unordered_map<uint64_t, VirtualStub> mVirtualStubs;
  std::queue<uint64_t> mVirtualStubsWorklist;
  std::set<uint64_t> mVirtualStubsExplored;

  std::set<uint64_t> updateEdges(std::map<uint64_t, std::set<uint64_t>> &Edges, const std::unordered_map<uint64_t, whitepeacock::BasicBlock> &VirtualBlocks, std::set<uint64_t> &VirtualBlocksExplored, whitepeacock::BasicBlock &Block);

  void identifyVirtualBlockDestinations(llvm::Function *F, std::set<uint64_t> &Destinations, const std::unordered_map<uint64_t, uint64_t> &ConstantsMap, size_t SolutionsLimit = 256, size_t SolverTimeout = 10, bool PrintSMTQuery = false);

  void identifyVirtualStubDestinations(VirtualStub &vmStub);

  void generateVirtualStubEpilogue(VirtualStub &vmStub);

  void splitExitBlock(VirtualStub &vmStub);

  void convertGetElementPtrToIntToPtr(llvm::Function *F);

  whitepeacock::BlockType isUnconditionalBasicBlock(const llvm::Function *F, uint64_t &destination) const;

};