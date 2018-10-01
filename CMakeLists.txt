cmake_minimum_required(VERSION 3.11)
project(Flipper C ASM)

set(CMAKE_INSTALL_PREFIX /usr/local)

add_subdirectory(library)
#add_subdirectory(carbon)

set_target_properties(flipper
        PROPERTIES
        COMPILER_FLAGS "-std=c99"
        COMPILER_FLAGS "-Wall"
        COMPILER_FLAGS "-Wextra"
        COMPILER_FLAGS "-Wno-unused-parameter"
        COMPILER_FLAGS "-Wno-expansion-to-defined"
        )
