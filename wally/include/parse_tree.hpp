#pragma once

#include <iostream>
#include <fstream>
using std::cout;
using std::endl;
using std::string;
using std::ofstream;

#include "grammar.hpp"
#include "AST.hpp"

#define FILE_EXT ".wly"
#define FILE_EXT_LEN 4

int validate(ptrNode& root, Program& program)
{

    // if(tab.length() > 10) return;
    for (auto &&child : root->children[0]->children[0]->children)
    {
        if(child == nullptr) continue;
        // if(child->is_type<wally::star_space>()) continue;
        // if(child->is_type<wally::star_nl_sp>()) continue;
        // if(child->is_type<wally::plus_nl_sp>()) continue;
        // if(child->is_type<wally::plus_nl>()) continue;
        // if(child->is_type<wally::one_nl>()) continue;
        // if(child->is_type<wally::comment>()) continue;
        // if(printAll || child->is_type<wally::main_states>())
        // {
        //     cout << tab << child->type <<endl;
        //     printTree(child, tab + '\t', true);
        // }
        // else
        // {
        //     printTree(child, tab + '\t');
        // }

        if(program.addState(child, program.prog_body))
        {
            return 1;
        }
    }
    return 0;
}
