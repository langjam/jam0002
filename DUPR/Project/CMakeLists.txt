cmake_minimum_required(VERSION 3.16)

project(dupr_LanguageProject
                      DESCRIPTION "Project for the language(s): dupr"
                      LANGUAGES CXX)

find_package(Deamer REQUIRED)

add_subdirectory(CompilerGenerator)
add_subdirectory(dupr)
