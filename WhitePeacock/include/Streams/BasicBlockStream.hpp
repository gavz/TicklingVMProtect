#pragma once

#include <Streams/BasicBlock.hpp>

struct BasicBlockStream {
  virtual bool getBasicBlock(uint64_t entryPoint, whitepeacock::BasicBlock &basicBlock) = 0;
};
