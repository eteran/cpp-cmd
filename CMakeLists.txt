cmake_minimum_required(VERSION 3.5)
project(cmd-cpp)

add_executable(cmd-example main.cpp)

target_link_libraries(cmd-example
PUBLIC
    cmd-cpp
)

target_compile_options(cmd-example
PUBLIC
    -W
    -Wall
    -Wunused
    -pedantic
)

set_property(TARGET cmd-example PROPERTY CXX_STANDARD 17)
set_property(TARGET cmd-example PROPERTY CXX_EXTENSIONS OFF)

add_subdirectory(lib)
