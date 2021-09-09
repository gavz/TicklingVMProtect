project(WhitePeacock)

# triton
find_package(TRITON REQUIRED)
list(APPEND PROJECT_LIBRARIES ${TRITON_LIBRARIES})
list(APPEND PROJECT_INCLUDES ${TRITON_INCLUDE_DIRS})
message(STATUS "Triton libraries: " ${TRITON_LIBRARIES})
message(STATUS "Triton includes: " ${TRITON_INCLUDE_DIRS})

# capstone
find_package(CAPSTONE REQUIRED)
list(APPEND PROJECT_LIBRARIES ${CAPSTONE_LIBRARIES})
list(APPEND PROJECT_INCLUDES ${CAPSTONE_INCLUDE_DIRS})
message(STATUS "Capstone libraries: " ${CAPSTONE_LIBRARIES})
message(STATUS "Capstone includes: " ${CAPSTONE_INCLUDE_DIRS})

# souper
find_package(SOUPER REQUIRED)
list(APPEND PROJECT_LIBRARIES ${SOUPER_LIBRARIES})
list(APPEND PROJECT_INCLUDES ${SOUPER_INCLUDE_DIRS})
message(STATUS "Souper libraries: " ${SOUPER_LIBRARIES})
message(STATUS "Souper includes: " ${SOUPER_INCLUDE_DIRS})

# alive2
find_package(ALIVE2 REQUIRED)
list(APPEND PROJECT_LIBRARIES ${ALIVE2_LIBRARIES})
list(APPEND PROJECT_INCLUDES ${ALIVE2_INCLUDE_DIRS})
message(STATUS "Alive2 libraries: " ${ALIVE2_LIBRARIES})
message(STATUS "Alive2 includes: " ${ALIVE2_INCLUDE_DIRS})

# z3
find_package(Z3 REQUIRED)
list(APPEND PROJECT_LIBRARIES ${Z3_LIBRARIES})
list(APPEND PROJECT_INCLUDES ${Z3_INCLUDE_DIRS})
message(STATUS "Z3 libraries: " ${Z3_LIBRARIES})
message(STATUS "Z3 includes: " ${Z3_INCLUDE_DIRS})

# hiredis
find_package(HIREDIS REQUIRED)
list(APPEND PROJECT_LIBRARIES ${HIREDIS_LIBRARIES})
list(APPEND PROJECT_INCLUDES ${HIREDIS_INCLUDE_DIRS})
message(STATUS "HIREDIS libraries: ${HIREDIS_LIBRARIES}")
message(STATUS "HIREDIS includes: ${HIREDIS_INCLUDE_DIRS}")

# boost
find_package(BOOST REQUIRED)
list(APPEND PROJECT_INCLUDES ${BOOST_INCLUDE_DIRS})
message(STATUS "Boost includes: " ${BOOST_INCLUDE_DIRS})

set(SOURCES
    src/Explorer/Explorer.cpp
    src/Utilities/Utility.cpp
    src/Lifter/Lifter.cpp
    src/Optimizer/Optimizer.cpp
    src/Streams/FileParserStream.cpp
    src/Streams/BinaryParserStream.cpp
    src/Snapshot/Snapshot.cpp
    src/Symbolizer/Symbolizer.cpp
    src/Translator/SVMTranslator.cpp
    src/Synthesis/SolverCache.cpp
    src/Synthesis/EnumerativeSynthesis.cpp
    src/Passes/SegmentsAAPass.cpp
    src/Passes/PointersHoistPass.cpp
    src/Passes/SynthesizeFlagsPass.cpp
    src/Passes/KnownIndexSelectPass.cpp
    src/Passes/MemoryCoalescingPass.cpp
    src/Passes/PartialOverlapDSEPass.cpp
    src/Passes/InstCombineGenericPass.cpp
    src/Passes/ConstantConcretizationPass.cpp
    )

set(HEADERS
    include/Explorer/Explorer.hpp
    include/Utilities/Utility.hpp
    include/Utilities/LLVMGlobalContext.hpp
    include/Lifter/Lifter.hpp
    include/Optimizer/Optimizer.hpp
    include/Streams/BasicBlock.hpp
    include/Streams/BasicBlockStream.hpp
    include/Streams/FileParserStream.hpp
    include/Streams/BinaryParserStream.hpp
    include/Snapshot/Snapshot.hpp
    include/Symbolizer/Symbolizer.hpp
    include/Translator/CFGStructures.hpp
    include/Translator/SVMTranslator.hpp
    include/Synthesis/SolverCache.hpp
    include/Synthesis/EnumerativeSynthesis.hpp
    include/Passes/SegmentsAAPass.hpp
    include/Passes/MemoryLocations.hpp
    include/Passes/PointersHoistPass.hpp
    include/Passes/SynthesizeFlagsPass.hpp
    include/Passes/KnownIndexSelectPass.hpp
    include/Passes/MemoryCoalescingPass.hpp
    include/Passes/PartialOverlapDSEPass.hpp
    include/Passes/InstCombineGenericPass.hpp
    include/Passes/ConstantConcretizationPass.hpp
    )

# BUILD_SHARED_LIBS option controls the library type
# https://cmake.org/cmake/help/latest/variable/BUILD_SHARED_LIBS.html#variable:BUILD_SHARED_LIBS
add_library(${PROJECT_NAME} ${SOURCES} ${HEADERS})

target_compile_options(${PROJECT_NAME} PUBLIC -fno-rtti)

target_include_directories(${PROJECT_NAME} PUBLIC include)
target_link_libraries(${PROJECT_NAME} PUBLIC LLVM REVIDE-Helpers)

# Add the dependency include directories
target_link_libraries(${PROJECT_NAME} PUBLIC ${PROJECT_LIBRARIES})
target_include_directories(${PROJECT_NAME} PUBLIC ${PROJECT_INCLUDES})