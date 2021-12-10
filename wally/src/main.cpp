#include <string>
#include <iostream>

#include "tao/pegtl.hpp"
#include <tao/pegtl/contrib/parse_tree.hpp>

#include "grammar.hpp"
#include "parse_tree.hpp"
#include "AST.hpp"

int main( int argc, char* argv[] )
{
    if( argc != 2 ) return 1;

    // Start a parsing run of argv[1] with the string
    // variable 'name' as additional argument to the
    // action; then print what the action put there.

    std::string name;
    std::string inFile(argv[1]);

    ptl::file_input in( inFile );
    string outFile(inFile);
    size_t start_pos = inFile.find(FILE_EXT);
    if(start_pos == string::npos || start_pos + FILE_EXT_LEN < inFile.length())
    {
        std::cerr << "File extension should be '.wly'\n";
        return 1;
    }
    outFile.replace(start_pos, FILE_EXT_LEN, ".c");

    ptrNode root;

    try
    {
        // ptl::parse< wally::grammar, wally::action >( in, name );
        root = (ptl::parse_tree::parse< wally::grammar, wally::selector >( in ));
        std::cout << "Parsing success!" << std::endl;
    }
    catch(const ptl::parse_error& e)
    {
        const auto p = e.positions().front();
        std::cerr << '\n' << e.what() << '\n';
        std::cerr << in.line_at(p) << '\n';
        std::cerr << std::setw( p.column ) << '^' << std::endl;
        std::cout << "Parsing error!" << std::endl;
        return 1;
    }



    Program program;
    if(validate(root, program))
    {
        std::cerr << "Errors found\n";
        return 1;
    }

    std::ofstream out( outFile );
    out << ("#include <stdio.h>\n\n") ;
    out << "int main()\n{\n";
    out << program.prog_body;
    out << "}\n";

    out.close();

    return 0;
}
