#pragma once

#include <llvm/IR/Module.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/raw_ostream.h>

struct LLVMGlobalContext {
  llvm::LLVMContext Context;
  std::shared_ptr<llvm::Module> Module;

  LLVMGlobalContext() = default;

  LLVMGlobalContext(const LLVMGlobalContext &) = delete;

  void ParseHelpersFile(const std::string &helpersFilePath) {
    llvm::SMDiagnostic Err;
    auto module = parseIRFile(helpersFilePath, Err, this->Context);
    if (!module) {
      throw std::runtime_error("failed to load the helpers file (" + helpersFilePath + ")");
    }
    this->Module = std::move(module);
  }

  void DumpFile(const std::string &dumpFile) {
    std::error_code err;
    llvm::raw_fd_ostream ofs(dumpFile, err);
    Module->print(ofs, nullptr);
  }
};
