project(Lifter)

add_executable(${PROJECT_NAME} main.cpp)

target_compile_options(${PROJECT_NAME} PUBLIC -fno-rtti)

target_link_libraries(${PROJECT_NAME} WhitePeacock)
