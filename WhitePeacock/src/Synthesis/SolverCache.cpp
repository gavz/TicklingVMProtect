#include <Synthesis/SolverCache.hpp>

#include <llvm/ADT/StringExtras.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/APInt.h>

#include <llvm/Support/PrettyStackTrace.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/CommandLine.h>

#include <cstddef>
#include <cstdlib>
#include <fstream>
#include <string>

llvm::cl::opt<bool> UseRedis("use-redis",
  llvm::cl::desc("Cache the SMT queries in a Redis database"),
  llvm::cl::value_desc("flag"),
  llvm::cl::init(false),
  llvm::cl::Optional);

SolverCache &SolverCache::Get() {
  static SolverCache mInstance;
  return mInstance;
}

SolverCache::SolverCache() {
  mSolver = getUnderlyingSolver();
  if (UseRedis) {
    struct timeval timeout = {1, 500000};  // 1.5 seconds
    mRedis = redisConnectWithTimeout("127.0.0.1", 6379, timeout);
    if (!mRedis) {
      llvm::report_fatal_error("[SolverCache] Can't allocate redis context\n");
    }
    if (mRedis->err) {
      llvm::report_fatal_error((llvm::StringRef) "[SolverCache] Redis connection error: " + mRedis->errstr + "\n");
    }
  }
}

void SolverCache::printSaturnSolverStats() {
  llvm::outs() << "[SolverCache] Solver queries: " << mSolverQueries
               << " Cache hits: " << (mCacheHits) << " ("
               << llvm::format("%0.2f",
                               ((float)(mCacheHits)*100. / (float)mSolverQueries))
               << "%) "
               << " Redis hits: " << mRedisHits
               << " Unique Replacements: " << mUniqueReplacements
               << " Missed cached/redis hits: "
               << mSolverQueries - mCacheHits - mRedisHits << "\n";
}

bool SolverCache::getSolverQuery(llvm::StringRef query, SolverQuery &solverQuery) {
  ++mSolverQueries;

  auto Q = mCache.find(query);
  if (Q == mCache.end())
    return false;

  ++mCacheHits;

  solverQuery = Q->second;

  return true;
}

bool SolverCache::getReplacement(llvm::StringRef query, std::string &result) {

  ++mSolverQueries;

  if (!mRedis)
    return false;

  if (hGet(query, "result", result)) {
    ++mRedisHits;
    return true;
  }

  return false;
}

void SolverCache::setReplacement(llvm::StringRef query, llvm::StringRef result) {

  if (!mRedis) return;

  hSet(query, "result", result);

  ++mUniqueReplacements;
}

void SolverCache::insertSolverQuery(llvm::StringRef query, bool isSat, std::vector<llvm::APInt> *model, std::error_code error, bool updateRedis) {
  // Update internal cache
  mCache[query] = SolverQuery(isSat, model, error);

  ++mUniqueReplacements;

  // Update redis only when no error exists
  if (!updateRedis || error || !mRedis)
    return;

  // Build result string
  std::string result = (isSat ? "1" : "0");

  // Add model
  if (model) {
    for (auto &I : *model)
      result += "," + llvm::utostr(I.getBitWidth()) + ":" + toString(I, 10, false);
  }

  // Update redis
  hSet(query, "result", result);
}

std::error_code SolverCache::isSatisfiableCached(llvm::StringRef query, bool &result, unsigned numModels, std::vector<llvm::APInt> *model, unsigned timeout) {
  std::error_code error;

  // Check if we have an in-memory cached result
  SolverQuery solverQuery;
  if (getSolverQuery(query, solverQuery)) {
    result = solverQuery.mIsSat;
    if (model) {
      for (auto &I : solverQuery.mModel)
        model->push_back(I);
    }
    return solverQuery.mError;
  }

  // Check if we have a Redis cached result
  std::string resultStr;
  if (mRedis && hGet(query, "result", resultStr)) {
    ++mRedisHits;
    if (!resultStr.empty()) {
      // Parse result
      llvm::SmallVector<llvm::StringRef, 8> VResult;
      llvm::StringRef R = resultStr;
      R.split(VResult, ",");

      // Set IsSat
      result = (VResult[0].trim() == "1");

      // Add models
      for (int i = 1; i < VResult.size(); i++) {
        auto RSplit = VResult[i].split(':');
        llvm::APInt I(std::stoi(RSplit.first.str()), RSplit.second, 10);
        model->push_back(I);
      }

      // Update internal cache
      insertSolverQuery(query, result, model, error, false);

      return error;
    }
  }

  // No cache entry or empty result so solve it
  error = mSolver.get()->isSatisfiable(query, result, numModels, model, timeout);

  // Update cache
  insertSolverQuery(query, result, model, error);

  return error;
}

bool SolverCache::hGet(llvm::StringRef key, llvm::StringRef field, std::string &value) {
  redisReply *reply = (redisReply *)redisCommand(mRedis, "HGET %s %s", key.data(), field.data());
  if (!reply || mRedis->err)
    llvm::report_fatal_error((llvm::StringRef) "[SolverCache] Redis error: " + mRedis->errstr);
  if (reply->type == REDIS_REPLY_NIL) {
    freeReplyObject(reply);
    return false;
  } else if (reply->type == REDIS_REPLY_STRING) {
    value = reply->str;
    freeReplyObject(reply);
    return true;
  } else {
    llvm::report_fatal_error(
      "[SolverCache] Redis protocol error for cache lookup, didn't expect "
      "reply type " +
      std::to_string(reply->type));
  }
}

void SolverCache::hSet(llvm::StringRef key, llvm::StringRef field, llvm::StringRef value) {
  redisReply *reply = (redisReply *)redisCommand(mRedis, "HSET %s %s %s", key.data(), field.data(), value.data());
  if (!reply || mRedis->err)
    llvm::report_fatal_error((llvm::StringRef) "Redis error: " + mRedis->errstr);
  if (reply->type != REDIS_REPLY_INTEGER) {
    llvm::outs() << "Key: " << key << "\n";
    llvm::outs() << "Value: " << value << "\n";
    llvm::outs() << "Str: " << reply->str << "\n";
    llvm::report_fatal_error(
      "[SolverCache] Redis protocol error for cache fill, didn't expect "
      "reply type " +
      std::to_string(reply->type));
  }
  freeReplyObject(reply);
}

souper::SMTLIBSolver *SolverCache::getSmtSolver() const {
  return mSolver.get();
}

std::unique_ptr<souper::SMTLIBSolver> SolverCache::getUnderlyingSolver() {
  std::string Z3PathStr("/usr/local/bin/z3");
  if (!std::filesystem::exists(Z3PathStr))
    llvm::report_fatal_error("Solver '" + Z3PathStr +
                             "' does not exist or is not executable.\nSet path "
                             "by -z3-path=<z3_binary_path>");
  return souper::createZ3Solver(souper::makeExternalSolverProgram(Z3PathStr), false);
}
