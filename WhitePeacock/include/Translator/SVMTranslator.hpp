#pragma

#include <Translator/CFGStructures.hpp>
#include <Symbolizer/Symbolizer.hpp>

#include <sstream>

using namespace std;

bool IdentifyHandler(Symbolizer *symbolizer, SharedVirtualInstruction &vmInstruction, bool is64Bit);