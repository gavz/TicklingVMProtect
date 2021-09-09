# Based on https://github.com/ttroy50/cmake-examples/tree/master/02-sub-projects/A-basic
# https://cliutils.gitlab.io/modern-cmake/
cmake_minimum_required(VERSION 3.16.4)

# Set the project name
project(VirtualMachineLifter)

# Append the CMake module search path so we can use our own modules
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/CMake)

# Project options
option(WHITE_PEACOCK_BUILD_TESTS "Build tests" OFF)
option(WHITE_PEACOCK_BUILD_HELPERS "Build helpers" OFF)
option(BUILD_SHARED_LIBS "Build shared libraries" OFF)

# Enable C++17
set(CMAKE_CXX_STANDARD 17)

# Enable the optimizations
if("${CMAKE_CXX_COMPILER_ID}" MATCHES "Clang" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
    set(CMAKE_CXX_FLAGS_RELEASE "-O3")
endif()

# LLVM wrapper
include(LLVM)

# MSVC-specific options
if(MSVC)
    # This assumes the installed LLVM was built in Release mode
    set(CMAKE_C_FLAGS_DEBUG "/ZI /Od /Ob0 /DNDEBUG" CACHE STRING "")
    set(CMAKE_CXX_FLAGS_DEBUG "/ZI /Od /Ob0 /DNDEBUG" CACHE STRING "")
    set(CMAKE_C_FLAGS_RELWITHDEBINFO "/ZI /Od /Ob0 /DNDEBUG" CACHE STRING "" FORCE)
    set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "/ZI /Od /Ob0 /DNDEBUG" CACHE STRING "" FORCE)

    if(${LLVM_USE_CRT_RELEASE} STREQUAL "MD")
        set(CMAKE_MSVC_RUNTIME_LIBRARY MultiThreadedDLL)
    elseif(${LLVM_USE_CRT_RELEASE} STREQUAL "MT")
        set(CMAKE_MSVC_RUNTIME_LIBRARY MultiThreaded)
    else()
        message(FATAL_ERROR "Unsupported LLVM_USE_CRT_RELEASE=${LLVM_USE_CRT_RELEASE}")
    endif()
endif()

# Helpers (requires clang++)
if(WHITE_PEACOCK_BUILD_HELPERS)
    add_subdirectory(Helpers)
endif()

add_subdirectory(ThirdParty/REVIDE)

# Main library
add_subdirectory(WhitePeacock)

# Lifter sample project
add_subdirectory(Lifter)

# Testing
if(WHITE_PEACOCK_BUILD_TESTS)
    set(DOCTEST_WITH_TESTS OFF CACHE BOOL "")
    set(DOCTEST_WITH_MAIN_IN_STATIC_LIB OFF CACHE BOOL "")
    add_subdirectory(ThirdParty/doctest EXCLUDE_FROM_ALL)

    add_subdirectory(Tests)
endif()
