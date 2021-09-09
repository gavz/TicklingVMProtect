#include <Snapshot/Snapshot.hpp>

Snapshot::Snapshot(API *Api, bool Is64Bit) : mApi(Api), mIs64Bit(Is64Bit) {
  mSnapshotTaintEngine = nullptr;
  mSnapshotSymEngine = nullptr;
  mCpu_x8664 = nullptr;
  mCpu_x86 = nullptr;
  mAstCtx = nullptr;
}

Snapshot::~Snapshot() {
  deleteSnapshot();
}

void Snapshot::takeSnapshot() {
  // Snapshot the symbolic engine
  mSnapshotSymEngine = new SymbolicEngine(*mApi->getSymbolicEngine());
  // Snapshot the taint engine
  mSnapshotTaintEngine = new TaintEngine(*mApi->getTaintEngine());
  // Snapshot the AST context
  mAstCtx = new AstContext(*mApi->getAstContext());
  // Snapshot the CPU context
  if (mIs64Bit) {
    mCpu_x8664 = new x8664Cpu(*(x8664Cpu *)(mApi->getCpuInstance()));
  } else {
    mCpu_x86 = new x86Cpu(*(x86Cpu *)(mApi->getCpuInstance()));
  }
}

void Snapshot::restoreSnapshot() {
  // Restore the symbolic engine
  *mApi->getSymbolicEngine() = *mSnapshotSymEngine;
  // Restore the taint engine
  *mApi->getTaintEngine() = *mSnapshotTaintEngine;
  // Restore the AST context
  *mApi->getAstContext() = *mAstCtx;
  // Restore the CPU context
  if (mIs64Bit)
    *(x8664Cpu *)(mApi->getCpuInstance()) = *mCpu_x8664;
  else
    *(x86Cpu *)(mApi->getCpuInstance()) = *mCpu_x86;
}

void Snapshot::deleteSnapshot() {
  if (mCpu_x86)
    delete mCpu_x86;
  if (mCpu_x8664)
    delete mCpu_x8664;
  if (mAstCtx)
    delete mAstCtx;
  if (mSnapshotTaintEngine)
    delete mSnapshotTaintEngine;
  if (mSnapshotSymEngine)
    delete mSnapshotSymEngine;
}