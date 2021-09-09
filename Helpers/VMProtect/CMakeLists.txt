# https://cliutils.gitlab.io/modern-cmake/chapters/basics/programs.html
project(VMProtectHelpers)

set(HELPER_CLANG_FLAGS
    -std=c++17
    -O3
    -Wno-gnu-inline-cpp-without-extern
    -fno-discard-value-names
    -fstrict-aliasing
    -fno-slp-vectorize
    -mllvm
    -enable-tbaa=true
    -emit-llvm
    -I../../ThirdParty/remill
    )

# Add the MacOS SDK sysroot and the LLVM C++ includes folder
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  list(APPEND HELPER_CLANG_FLAGS "-isysroot${CMAKE_OSX_SYSROOT}")
  list(APPEND HELPER_CLANG_FLAGS "-I${LLVM_INCLUDE_DIR}/c++/v1")
endif()

set(SOURCES
    VMProtectHelpers.cpp
    LLVMFlags.h
    )

# Remill requires you to define ADDRESS_SIZE_BITS because it defaults to 64
add_custom_command(OUTPUT
    "${CMAKE_CURRENT_SOURCE_DIR}/VMProtectHelpers32.ll"
    "${CMAKE_CURRENT_SOURCE_DIR}/VMProtectHelpers32.bc"
    "${CMAKE_CURRENT_SOURCE_DIR}/VMProtectHelpers64.ll"
    "${CMAKE_CURRENT_SOURCE_DIR}/VMProtectHelpers64.bc"
    COMMAND "${CLANG_EXECUTABLE}" ${HELPER_CLANG_FLAGS} -DADDRESS_SIZE_BITS=32 -m32 -S VMProtectHelpers.cpp -o VMProtectHelpers32.ll
    COMMAND "${CLANG_EXECUTABLE}" ${HELPER_CLANG_FLAGS} -DADDRESS_SIZE_BITS=32 -m32 -c VMProtectHelpers.cpp -o VMProtectHelpers32.bc
    COMMAND "${CLANG_EXECUTABLE}" ${HELPER_CLANG_FLAGS} -DADDRESS_SIZE_BITS=64 -m64 -S VMProtectHelpers.cpp -o VMProtectHelpers64.ll
    COMMAND "${CLANG_EXECUTABLE}" ${HELPER_CLANG_FLAGS} -DADDRESS_SIZE_BITS=64 -m64 -c VMProtectHelpers.cpp -o VMProtectHelpers64.bc
    MAIN_DEPENDENCY VMProtectHelpers.cpp
    DEPENDS ${SOURCES}
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
    )

add_custom_target(${PROJECT_NAME} ALL
    DEPENDS VMProtectHelpers32.ll VMProtectHelpers32.bc VMProtectHelpers64.ll VMProtectHelpers64.bc
    SOURCES ${SOURCES}
    )
