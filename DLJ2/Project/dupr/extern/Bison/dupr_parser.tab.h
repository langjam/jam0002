/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_DUPR_DUPR_PARSER_TAB_H_INCLUDED
# define YY_DUPR_DUPR_PARSER_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int duprdebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    LEFT_BRACKET = 258,            /* LEFT_BRACKET  */
    RIGHT_BRACKET = 259,           /* RIGHT_BRACKET  */
    LEFT_PARANTHESIS = 260,        /* LEFT_PARANTHESIS  */
    RIGHT_PARANTHESIS = 261,       /* RIGHT_PARANTHESIS  */
    LEFT_SQUARE_BRACKET = 262,     /* LEFT_SQUARE_BRACKET  */
    RIGHT_SQUARE_BRACKET = 263,    /* RIGHT_SQUARE_BRACKET  */
    ADD = 264,                     /* ADD  */
    MINUS = 265,                   /* MINUS  */
    MULTI = 266,                   /* MULTI  */
    DIVIDE = 267,                  /* DIVIDE  */
    LT = 268,                      /* LT  */
    LE = 269,                      /* LE  */
    GT = 270,                      /* GT  */
    GE = 271,                      /* GE  */
    EQ = 272,                      /* EQ  */
    OR = 273,                      /* OR  */
    AND = 274,                     /* AND  */
    DOT = 275,                     /* DOT  */
    COMMA = 276,                   /* COMMA  */
    COLON = 277,                   /* COLON  */
    SEMICOLON = 278,               /* SEMICOLON  */
    SIGN = 279,                    /* SIGN  */
    HEKJE = 280,                   /* HEKJE  */
    QUESTION = 281,                /* QUESTION  */
    EXCLAM = 282,                  /* EXCLAM  */
    PATTERN_INSERTION = 283,       /* PATTERN_INSERTION  */
    VARNAME = 284,                 /* VARNAME  */
    NUMBER = 285,                  /* NUMBER  */
    DECIMAL = 286                  /* DECIMAL  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 127 "./dupr_parser.y"

	::deamer::external::cpp::lexer::TerminalObject* Terminal;
	::dupr::ast::node::LEFT_BRACKET* dupr_LEFT_BRACKET;
	::dupr::ast::node::RIGHT_BRACKET* dupr_RIGHT_BRACKET;
	::dupr::ast::node::LEFT_PARANTHESIS* dupr_LEFT_PARANTHESIS;
	::dupr::ast::node::RIGHT_PARANTHESIS* dupr_RIGHT_PARANTHESIS;
	::dupr::ast::node::LEFT_SQUARE_BRACKET* dupr_LEFT_SQUARE_BRACKET;
	::dupr::ast::node::RIGHT_SQUARE_BRACKET* dupr_RIGHT_SQUARE_BRACKET;
	::dupr::ast::node::ADD* dupr_ADD;
	::dupr::ast::node::MINUS* dupr_MINUS;
	::dupr::ast::node::MULTI* dupr_MULTI;
	::dupr::ast::node::DIVIDE* dupr_DIVIDE;
	::dupr::ast::node::LT* dupr_LT;
	::dupr::ast::node::LE* dupr_LE;
	::dupr::ast::node::GT* dupr_GT;
	::dupr::ast::node::GE* dupr_GE;
	::dupr::ast::node::EQ* dupr_EQ;
	::dupr::ast::node::OR* dupr_OR;
	::dupr::ast::node::AND* dupr_AND;
	::dupr::ast::node::DOT* dupr_DOT;
	::dupr::ast::node::COMMA* dupr_COMMA;
	::dupr::ast::node::COLON* dupr_COLON;
	::dupr::ast::node::SEMICOLON* dupr_SEMICOLON;
	::dupr::ast::node::SIGN* dupr_SIGN;
	::dupr::ast::node::HEKJE* dupr_HEKJE;
	::dupr::ast::node::QUESTION* dupr_QUESTION;
	::dupr::ast::node::EXCLAM* dupr_EXCLAM;
	::dupr::ast::node::PATTERN_INSERTION* dupr_PATTERN_INSERTION;
	::dupr::ast::node::VARNAME* dupr_VARNAME;
	::dupr::ast::node::NUMBER* dupr_NUMBER;
	::dupr::ast::node::DECIMAL* dupr_DECIMAL;
	::dupr::ast::node::ESCAPE_CHARS* dupr_ESCAPE_CHARS;
	::dupr::ast::node::program* dupr_program;
	::dupr::ast::node::deamerreserved_star__stmt__* dupr_deamerreserved_star__stmt__;
	::dupr::ast::node::stmt* dupr_stmt;
	::dupr::ast::node::pattern_execution* dupr_pattern_execution;
	::dupr::ast::node::pattern_constructor_array* dupr_pattern_constructor_array;
	::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____* dupr_deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____;
	::dupr::ast::node::pattern_constructor* dupr_pattern_constructor;
	::dupr::ast::node::pattern_type* dupr_pattern_type;
	::dupr::ast::node::pattern_name* dupr_pattern_name;
	::dupr::ast::node::deamerreserved_arrow__VARNAME__* dupr_deamerreserved_arrow__VARNAME__;
	::dupr::ast::node::deamerreserved_star__GT__VARNAME__* dupr_deamerreserved_star__GT__VARNAME__;
	::dupr::ast::node::pattern_constructor_content* dupr_pattern_constructor_content;
	::dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__* dupr_deamerreserved_star__pattern_constructor_content_stmt__;
	::dupr::ast::node::pattern_constructor_content_stmt* dupr_pattern_constructor_content_stmt;
	::dupr::ast::node::pattern_constructor_operator* dupr_pattern_constructor_operator;
	::dupr::ast::node::pattern_constructor_structure* dupr_pattern_constructor_structure;
	::dupr::ast::node::pattern_constructor_terminate* dupr_pattern_constructor_terminate;
	::dupr::ast::node::pattern_constructor_encapsulation* dupr_pattern_constructor_encapsulation;

#line 147 "dupr_parser.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE duprlval;


int duprparse (void);


#endif /* !YY_DUPR_DUPR_PARSER_TAB_H_INCLUDED  */
