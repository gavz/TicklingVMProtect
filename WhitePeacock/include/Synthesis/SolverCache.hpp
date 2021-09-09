#pragma once

#include <llvm/ADT/StringMap.h>
#include <llvm/IR/Value.h>

#include <llvm/Support/CommandLine.h>
#include <llvm/Support/raw_ostream.h>

#include <souper/Extractor/Solver.h>

#include <hiredis/hiredis.h>

// In-memory caching structure
typedef struct SolverQuery {
  bool mIsSat;
  std::error_code mError;
  std::vector<llvm::APInt> mModel;

  SolverQuery(bool isSat, std::vector<llvm::APInt> *model, std::error_code error) {
    mError = error;
    mIsSat = isSat;

    // Clone APInts
    if (!model) return;
    for (auto &I : *model)
      mModel.push_back(I);
  }

  SolverQuery() {}
} SolverQuery;

class SolverCache {
public:

  // Gets the static instance
  static SolverCache &Get();

  // Return the SMT solver instance
  souper::SMTLIBSolver *getSmtSolver() const;

  // Prints the statistics
  void printSaturnSolverStats();

  // Returns true on cache hit
  bool getSolverQuery(llvm::StringRef query, SolverQuery &solverQuery);

  // Returns true on cache hit
  bool getReplacement(llvm::StringRef query, std::string &result);

  // Return true on cache hit
  void setReplacement(llvm::StringRef query, llvm::StringRef result);

  // Inserts a new query/result into the cache
  void insertSolverQuery(llvm::StringRef query, bool isSat,
    std::vector<llvm::APInt> *model, std::error_code error = std::error_code(), bool updateRedis = true);

  // Cached query isSatisfiable method
  std::error_code isSatisfiableCached(llvm::StringRef query, bool &result, unsigned numModels,
    std::vector<llvm::APInt> *model, unsigned timeout);

 private:

  SolverCache();

  SolverCache(SolverCache const&) = delete;

  void operator=(SolverCache const&) = delete;

  bool hGet(llvm::StringRef Key, llvm::StringRef Field, std::string &Value);
  void hSet(llvm::StringRef Key, llvm::StringRef Field, llvm::StringRef Value);

  std::unique_ptr<souper::SMTLIBSolver> getUnderlyingSolver();

  // Statistics
  int mCacheHits = 0;
  int mRedisHits = 0;
  int mSolverQueries = 0;
  int mUniqueReplacements = 0;

  // Redis context
  redisContext *mRedis = nullptr;

  // Underlying SMT solver
  std::unique_ptr<souper::SMTLIBSolver> mSolver;

  // In-memory solver cache
  llvm::StringMap<SolverQuery> mCache;
};