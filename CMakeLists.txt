cmake_minimum_required(VERSION 3.10)

project(chc C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g")

include_directories(${CMAKE_SOURCE_DIR}/src/include)

add_subdirectory(src)
