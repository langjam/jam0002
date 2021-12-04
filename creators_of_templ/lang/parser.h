// A parser

#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "ast.h"


typedef struct Parser {
    Lexer lexer;
    Token current;
    Ast ast;
    Err err;
} Parser;

// Creates a new parser from source code
Parser parser_init(char *src);

// Runs the parser, and returns non-zero error code if something happened
// Check parser->err to find the details of the error
ErrCode parser_run(Parser *p);

// Frees data from parser
void parser_deinit(Parser *p);

#endif // PARSER_H

