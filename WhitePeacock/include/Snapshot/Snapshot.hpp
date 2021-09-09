#pragma once

#include <triton/symbolicEngine.hpp>
#include <triton/taintEngine.hpp>
#include <triton/x8664Cpu.hpp>
#include <triton/x86Cpu.hpp>
#include <triton/ast.hpp>
#include <triton/api.hpp>

using namespace triton::engines::symbolic;
using namespace triton::engines::taint;
using namespace triton::arch::x86;
using namespace triton::ast;
using namespace triton;

class Snapshot {
  private:
    // Main Triton API
    API *mApi;
    // Snapshot of the symbolic engine
    SymbolicEngine *mSnapshotSymEngine;
    // Snapshot of the taint engine
    TaintEngine *mSnapshotTaintEngine;
    // AST Context
    AstContext *mAstCtx;
    // Snapshot of triton CPU
    x8664Cpu *mCpu_x8664;
    x86Cpu *mCpu_x86;
    // Bitness
    bool mIs64Bit;
  public:
    // Constructor
    Snapshot(API* Api, bool Is64Bit);
    // Destructor
    ~Snapshot();
    // Restores a snapshot
    void restoreSnapshot();
    // Remove a snapshot
    void deleteSnapshot();
    // Takes a snapshot
    void takeSnapshot();
};