#include <Utilities/LLVMGlobalContext.hpp>
#include <Streams/BinaryParserStream.hpp>
#include <Explorer/Explorer.hpp>
#include <Lifter/Lifter.hpp>

#include <llvm/Support/CommandLine.h>
#include <llvm/Support/InitLLVM.h>

// Command line options

llvm::cl::opt<std::string>
  InputFilename("i", llvm::cl::desc("VMProtected input binary"),
  llvm::cl::value_desc("filename"), llvm::cl::Required);

llvm::cl::opt<std::string> HelpersPath("h",
  llvm::cl::desc("Path to the compiled LLVM helpers"),
  llvm::cl::value_desc("path"), llvm::cl::Required);

llvm::cl::list<uint64_t> EntryPoints("f",
  llvm::cl::OneOrMore,
  llvm::cl::desc("Comma separated list of function begin VAs to devirtualize"),
  llvm::cl::value_desc("addresses"),
  llvm::cl::CommaSeparated);

// Hijacks the command line to add more LLVM options

static void hijackCommandLineArguments(int argc, char **argv, int &argc_new, char **&argv_new) {
  // Lambda function to add a new argument
  auto addArgument = [](std::vector<char *> &argv_vec, char *argument) {
    llvm::outs() << "[!] Adding command line argument: " << argument << "\n";
    argv_vec.push_back(argument);
  };
  // Copy the original command line
  std::vector<char *> argv_vec;
  for (int i = 0; i < argc; i++)
    argv_vec.push_back(argv[i]);
  // EarlyCSE
  addArgument(argv_vec, (char *)"-earlycse-mssa-optimization-cap=1000000");
  // DeadStoreElimination
  addArgument(argv_vec, (char *)"-dse-memoryssa-defs-per-block-limit=1000000");
  addArgument(argv_vec, (char *)"-dse-memoryssa-partial-store-limit=1000000");
  addArgument(argv_vec, (char *)"-dse-memoryssa-path-check-limit=1000000");
  addArgument(argv_vec, (char *)"-dse-memoryssa-scanlimit=1000000");
  addArgument(argv_vec, (char *)"-dse-memoryssa-walklimit=1000000");
  addArgument(argv_vec, (char *)"-dse-memoryssa-otherbb-cost=2");
  // MemorySSA
  addArgument(argv_vec, (char *)"-memssa-check-limit=1000000");
  // MemoryDepencenceAnalsys
  addArgument(argv_vec, (char *)"-memdep-block-number-limit=1000000");
  addArgument(argv_vec, (char *)"-memdep-block-scan-limit=1000000");
  // GVN
  addArgument(argv_vec, (char *)"-gvn-max-block-speculations=1000000");
  addArgument(argv_vec, (char *)"-gvn-max-num-deps=1000000");
  // GVNHoist
  addArgument(argv_vec, (char *)"-gvn-hoist-max-chain-length=-1");
  addArgument(argv_vec, (char *)"-gvn-hoist-max-depth=-1");
  addArgument(argv_vec, (char *)"-gvn-hoist-max-bbs=-1");
  // LoopUnroll
  addArgument(argv_vec, (char *)"-unroll-threshold=1000000");
  // Generate the updated argv pointer
  argc_new = argv_vec.size();
  argv_new = new char*[argc_new];
  for (size_t i = 0; i < argv_vec.size(); i++)
    argv_new[i] = argv_vec[i];
}

// Entry point

int main(int argc, char **argv) {
  // Initialize the LLVM arguments
  llvm::InitLLVM LLVMArgs(argc, argv);

  // Hijack the command line with additional arguments
  int argc_new = 0;
  char **argv_new = nullptr;
  hijackCommandLineArguments(argc, argv, argc_new, argv_new);

  // Parse the command line arguments
  llvm::cl::ParseCommandLineOptions(argc_new, argv_new);

  // Print the information
  llvm::outs() << "[+] Opening: " << InputFilename << "\n";
  llvm::outs() << "[+] Helpers: " << HelpersPath << "\n";

  // Initialize the global LLVM context
  LLVMGlobalContext ctx;
  ctx.ParseHelpersFile(HelpersPath);

  // Initialize the LLVM lifter
  Lifter lifter(ctx.Module);

  // Parse the binary
  BinaryParserStream *bps = new BinaryParserStream(InputFilename);

  // Lift the virtualized functions
  try {
    for (const uint64_t address : EntryPoints) {
      Explorer explorer(address, lifter, *bps);
      explorer.run();
    }
  } catch (const std::exception &ex) {
    llvm::errs() << "Exception thrown: " << ex.what() << "\n";
  }

  // Deallocate argv_new
  delete[] argv_new;

  // Exit successfully
  return EXIT_SUCCESS;
}