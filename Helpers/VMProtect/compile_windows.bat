@echo off

set LLVMPATH=%~dp0..\..\..\llvm10-install-full-v2
set TBAAFLAG=true
set WARNINGS=-Wno-gnu-inline-cpp-without-extern

"%LLVMPATH%"\bin\clang++ -O3 %WARNINGS% -fno-discard-value-names -fstrict-aliasing -fno-slp-vectorize -mllvm -enable-tbaa=%TBAAFLAG% VMProtectHelpers.cpp -emit-llvm -S -I..\..\ThirdParty\remill -std=c++17 -o VMProtectHelpers64.ll
"%LLVMPATH%"\bin\clang++ -O3 %WARNINGS% -fno-discard-value-names -fstrict-aliasing -fno-slp-vectorize -mllvm -enable-tbaa=%TBAAFLAG% VMProtectHelpers.cpp -emit-llvm -c -I..\..\ThirdParty\remill -std=c++17 -o VMProtectHelpers64.bc
"%LLVMPATH%"\bin\clang++ -O3 %WARNINGS% -fno-discard-value-names -fstrict-aliasing -fno-slp-vectorize -mllvm -enable-tbaa=%TBAAFLAG% VMProtectHelpers.cpp -m32 -emit-llvm -S -I..\..\ThirdParty\remill -std=c++17 -o VMProtectHelpers32.ll
"%LLVMPATH%"\bin\clang++ -O3 %WARNINGS% -fno-discard-value-names -fstrict-aliasing -fno-slp-vectorize -mllvm -enable-tbaa=%TBAAFLAG% VMProtectHelpers.cpp -m32 -emit-llvm -c -I..\..\ThirdParty\remill -std=c++17 -o VMProtectHelpers32.bc