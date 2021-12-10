#include "AST.hpp"


void printTree(ptrNode& root, string tab)
{
    for (auto &&child : root->children)
    {
        {
            //  cout << tab << child->type <<endl;
        }
        printTree(child, tab + '\t');
    }
    
}


Variable::Variable(ptrNode& node)
{
    // //  cout << "var decl\n";
    // if(node->is_type<wally::var_asgn>()) printTree(node, "");
    // //  cout << "var decl done\n";

    for (auto &&child : node->children)
    {
        // //  cout << "Pattern " << child->type << "\n";

        if(child->is_type<ptl::opt<wally::pattern>>())
        {
            // add pattern
            // printTree(child, "");
            pattern = child->string();
            if(!pattern.empty()) pattern.pop_back();
            //  cout << "Pattern " << pattern  << "\n";
        }
        if(child->is_type<wally::variable>())
        {
            // add pattern
            //  cout << "var name " << child->string() << "\n";
            // printTree(child, "");
            name = child->string();
            
        }
        if(child->is_type<
                ptl::opt<tao::pegtl::ascii::string<'='>, wally::star_space, wally::expression>
            >())
        {
            if(!child->string().empty())
            {
                //  cout << "init expr " << child->string() << "\n";
                // //  cout << "init expr " << child->children.size() << "\n";
                // printTree(child, "");
                initExpr = child->children.back()->string();
                
            }
        }
        //TODO:
        if(child->is_type<wally::expression>())
        {
            if(!child->string().empty())
            {
                //  cout << "asgn expr " << child->string() << "\n";
                // //  cout << "asgn expr " << child->children.size() << "\n";
                // printTree(child, "");
                initExpr = child->string();
                
            }
        }
    }
    
}



Program::Program()
{
    // funcs[0].push_back(Function("main"));
}

Variable Program::getVar(string name)
{
    for (auto &&eachVar : vars)
    {
        if(eachVar.name == name)
        {
            return eachVar;
        }
    }

    return Variable();
}

// void addVar(string name, string pattern)
// {
//     vars[scope].push_back(Variable(name, pattern));
// }
void Program::addVar(ptrNode& varNode, string& body)
{
    Variable var(varNode);
    vars.push_back(var);
    body += tabs + "double " + var.name;
    if(!var.initExpr.empty())
    {
        body += " = " + var.initExpr;
    }
    else
    {
        body += " = 0";
    }
    body += ";\n";
}

int Program::addVarAsgn(ptrNode& varNode, string& body)
{
    Variable varAsgn(varNode);
    Variable ogVar = getVar(varAsgn.name);

    if(ogVar.name !=varAsgn.name)
    {
        std::cerr << "Variable '" << varAsgn.name << "' not found\n";
        return 1;
    }
    if(ogVar.pattern !=varAsgn.pattern)
    {
        std::cerr << "Pattern mismatch for variable '" << varAsgn.name << "'\n";
        return 1;
    }

    body += tabs + ogVar.name;
    body += " = " + varAsgn.initExpr;
    body += ";\n";
    return 0;
}

int Program::addState(ptrNode& child, string& body)
{
    if(child->is_type<wally::var_decl>())
    {
        //  cout << child->type <<endl;
        addVar(child, body);
    }
    else if(child->is_type<wally::var_asgn>())
    {
        //  cout << child->type <<endl;
        if(addVarAsgn(child, body))
        {
            return 1;
        }
    }
    else if(child->is_type<wally::print_call>())
    {
        //  cout << child->type <<endl;
        //  cout << child->string() << endl;
        printTree(child, "");
        //  cout << child->children[4]->string() << endl;

        body += tabs + "printf(\"%f\\n\", " + child->children[4]->string() + ");\n";
    }
    else if(child->is_type<wally::if_cond>() || child->is_type<wally::while_cond>())
    {
        //  cout << child->type <<endl;
        //  cout << child->string() << endl;
        // printTree(child, "");

        string condition = "";
        condition += child->children[4]->string() + " ";
        condition += child->children[6]->string();

        //  cout << "cond " << condition << endl;

        string if_body = tabs + child->children[0]->string();
        if_body += " ( " + condition + " )\n" + tabs + "{\n";

        auto& bodyNode = child->children[11];
        // printTree(bodyNode, "\t");
        tabs += '\t';
        for (auto &&bodyChild : bodyNode->children)
        {
            addState(bodyChild, if_body);
        }
        tabs.pop_back();

        if_body += tabs + "}\n";

        //  cout  << "if body " << if_body << endl;

        body += if_body;
        // body += "\tprintf(\"%f\\n\", " + child->children[2]->string() + ");\n";
    }
    // else if(child->is_type<wally::func_decl>())
    // {
    //     //  cout << child->type <<endl;
    //     if(addFunc(child))
    //     {
    //         return 1;
    //     }
    // }
    else if(child->is_type<wally::main_states>())
    {
        //  cout << child->type <<endl;
        // printTree(child, "\t");
        // return addState(child->children[0]->children[0]->children[0]);
        if(!child->children[0]->is_type<wally::comment>())
        {
            return addState(child->children[0]->children[0]->children[0], body);
        }
    }
    else if(child->is_type<wally::secon_states>())
    {
        //  cout << child->type <<endl;
        // printTree(child, "\t");
        // return addState(child->children[0]->children[0]->children[0]);
        if(!child->children[0]->is_type<wally::comment>())
        {
            return addState(child->children[0], body);
        }
    }
    // else if(child->is_type<tao::pegtl::sor<wally::var_asgn, wally::var_decl, wally::if_cond, wally::while_cond>>())
    // {
    //     // //  cout << child->type <<endl;
    //     // printTree(child, "\t");
    //     return addState(child->children[0]);
    // }

    return 0;
}


