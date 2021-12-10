#pragma once

#include <vector>
#include <map>
#include <string>
#include <sstream>
#include <fstream>
#include <iostream>
using std::vector;
using std::map;
using std::string;
using std::ofstream;
using std::stringstream;
using std::cerr;
using std::cout;
using std::endl;


#include "tao/pegtl.hpp"
#include <tao/pegtl/contrib/parse_tree.hpp>
#define Node tao::pegtl::parse_tree::node
#define ptrNode std::unique_ptr<tao::pegtl::parse_tree::node>

#include "grammar.hpp"

void printTree(ptrNode& root, string tab);


class Variable
{
private:
    /* data */
public:
    string name, pattern, initExpr;
    Variable(string _name, string _pat) : name(_name), pattern(_pat) {}
    Variable() : name(), pattern() {}
    Variable(ptrNode& node);
};


class Program
{
private:

public:
    /* data */
    vector<Variable> vars;
    string prog_body, tabs = "\t";
    // map<int, vector<Function>> funcs;

    Program();

    Variable getVar(string name);

    // void addVar(string name, string pattern);
    void addVar(ptrNode& varNode, string& body);
    int addVarAsgn(ptrNode& varNode, string& body);
    // int addFunc(ptrNode& funcNode);
    int addState(ptrNode& child, string& body);

};

// class Function
// {
//     public:
//     string name;
//     vector<string> params;
//     string body;
//     // stringstream body;
//     Function(string _na):name(_na){}
//     Function(ptrNode& node, Program* program)
//     {
//         for (auto &&child : node->children)
//         {
//             // cout << "f " << child->type << "\n";

//             if(child->is_type<wally::func_name_decl>())
//             {
//                 name = child->string();
//                 cout << "func name " << name << endl;
//             }
//             else if(child->is_type<wally::func_args_decl>())
//             {
//                 name = child->string();
//                 cout << "func args " << name << endl;
//                 // printTree(child, "\t");

//                 // 1st param
//                 // cout << "ading param type " << child->children[0]->type << "\n";
//                 params.push_back(child->children[0]->string());
//                 cout << "add param " << params.back() << "\n";

//                 for (auto &&vars : child->children[1]->children)
//                 {
//                     if(vars->is_type<wally::variable>())
//                     {
//                         // cout << "ading param type " << vars->type << "\n";
//                         params.push_back(vars->string());
//                         cout << "add param " << params.back() << "\n";
//                     }
//                 }
                
//             }
//             else if(child->is_type<wally::func_body_decl>())
//             {
//                 printTree(child, "");
//                 for (auto &&state : child->children)
//                 {
//                     program->addState(state);
//                 }
//             }
//         }
//     }
// };
