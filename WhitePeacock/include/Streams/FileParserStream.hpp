#pragma once

#include <istream>
#include <map>

#include <Streams/BasicBlockStream.hpp>

struct FileParserStream : BasicBlockStream {
  std::map<uint64_t, whitepeacock::BasicBlock> BasicBlocks;

  void parse(std::istream &stream);

  bool getBasicBlock(uint64_t entryPoint, whitepeacock::BasicBlock &basicBlock) override;
};
