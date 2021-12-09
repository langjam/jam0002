# This file is auto-generated and auto-maintained.
# Please don't change code in this file, as new changes will be discarded,
# when Deamer regenerates the project.
#
# If you want to contribute to Deamer.
# Please visit: https://github.com/Deruago/theDeamerProject 
#
# This file is part of the directory: Bison

set(source_files ./dupr_parser.tab.cpp)
dupr_add_external_library(Bison ${source_files})
target_link_libraries(dupr_Bison_static_library PUBLIC dupr_Flex_static_library)
