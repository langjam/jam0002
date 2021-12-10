cmake_minimum_required(VERSION 3.16)
project(dupr_CompilerGenerator
                      DESCRIPTION "Compiler/Ecosystem generator for language(s): dupr. This CompilerGenerator project uses Deamer CC for the generation, and DLDL to generate the definitions."
                      LANGUAGES CXX)
find_package(Deamer REQUIRED)
file(GLOB_RECURSE SOURCE_LIST "${dupr_CompilerGenerator_SOURCE_DIR}/lib/*.cpp")
add_library(dupr_CompilerGenerator_static_library STATIC ${SOURCE_LIST})
target_include_directories(dupr_CompilerGenerator_static_library PUBLIC "${dupr_CompilerGenerator_SOURCE_DIR}/include")
target_link_libraries(dupr_CompilerGenerator_static_library PUBLIC Deamer)
target_compile_features(dupr_CompilerGenerator_static_library PUBLIC cxx_std_17)
set_target_properties(dupr_CompilerGenerator_static_library PROPERTIES LINKER_LANGUAGE CXX)
add_executable(dupr_CompilerGenerator_Main "${dupr_CompilerGenerator_SOURCE_DIR}/main.cpp")
target_link_libraries(dupr_CompilerGenerator_Main PUBLIC dupr_CompilerGenerator_static_library)
