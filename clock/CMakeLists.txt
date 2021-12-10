cmake_minimum_required(VERSION 3.15)
project(clock)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_EXTENSIONS ON)

set(DCMAKE_CXX_FLAGS_RELEASE "-O3 -flto -DNDEBUG -Wall -Wextra")
set(DCMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -flto -DNDEBUG -Wall -Wextra -Werror")
set(DCMAKE_CXX_FLAGS_DEBUG "-Og -g -DDEBUG -fsanitize=address -fsanitize=undefined -Wall -Wextra")
add_compile_options(-fno-exceptions -Wall -Wextra -fdiagnostics-color -Werror)

add_executable(clock main.cpp)
target_include_directories(clock PUBLIC neo)
