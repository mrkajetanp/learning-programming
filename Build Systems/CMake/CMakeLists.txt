# minimum version and compatibility on cygwin
cmake_minimum_required (VERSION 2.8 FATAL_ERROR)
set (CMAKE_LEGACY_CYGWIN_WIN32 0)

# Set c++ standard
set (CMAKE_CXX_STANDARD 14)

# Project name
project ("ToDo List")

# Enable tests
enable_testing ()

# Setting compiler flags according to platform
if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" OR
    "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    set(warnings "-Wall -Wextra -Werror")
elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "MSVC")
    set(warnings "/W4 /WX /EHsc")
endif()
if (NOT CONFIGURED_ONCE)
    set(CMAKE_CXX_FLAGS "${warnings}"
        CACHE STRING "Flags used by the compiler during all build types." FORCE)
    set(CMAKE_C_FLAGS   "${warnings}"
        CACHE STRING "Flags used by the compiler during all build types." FORCE)
endif()

# include subdirectories from cmake source dir
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

add_subdirectory (src)

# program executable
# add_executable (toDo main.cpp)

# link library to main executable
# target_link_libraries (toDo toDoCore)

# test executable
# add_test (toDoTest toDo)

set (CONFIGURED_ONCE TRUE CACHE INTERNAL "A flag showing that CMake has configured at least once.")
