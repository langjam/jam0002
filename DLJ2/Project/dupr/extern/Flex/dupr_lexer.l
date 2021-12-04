%option yylineno
%option noyywrap
%option never-interactive
%option nounistd

%{
#include <iostream>
#include <string>
#include <stdio.h>
#include <string.h>
#include <vector>
#include <Deamer/External/Cpp/Lexer/TerminalObject.h>
#include "dupr/Flex/Lexer.h"
#include "dupr/Ast/Enum/Type.h"
#include "dupr/Ast/Node/LEFT_BRACKET.h"
#include "dupr/Ast/Node/RIGHT_BRACKET.h"
#include "dupr/Ast/Node/LEFT_PARANTHESIS.h"
#include "dupr/Ast/Node/RIGHT_PARANTHESIS.h"
#include "dupr/Ast/Node/LEFT_SQUARE_BRACKET.h"
#include "dupr/Ast/Node/RIGHT_SQUARE_BRACKET.h"
#include "dupr/Ast/Node/ADD.h"
#include "dupr/Ast/Node/MINUS.h"
#include "dupr/Ast/Node/MULTI.h"
#include "dupr/Ast/Node/DIVIDE.h"
#include "dupr/Ast/Node/LT.h"
#include "dupr/Ast/Node/LE.h"
#include "dupr/Ast/Node/GT.h"
#include "dupr/Ast/Node/GE.h"
#include "dupr/Ast/Node/EQ.h"
#include "dupr/Ast/Node/OR.h"
#include "dupr/Ast/Node/AND.h"
#include "dupr/Ast/Node/DOT.h"
#include "dupr/Ast/Node/COMMA.h"
#include "dupr/Ast/Node/COLON.h"
#include "dupr/Ast/Node/SEMICOLON.h"
#include "dupr/Ast/Node/SIGN.h"
#include "dupr/Ast/Node/HEKJE.h"
#include "dupr/Ast/Node/QUESTION.h"
#include "dupr/Ast/Node/EXCLAM.h"
#include "dupr/Ast/Node/PATTERN_INSERTION.h"
#include "dupr/Ast/Node/VARNAME.h"
#include "dupr/Ast/Node/NUMBER.h"
#include "dupr/Ast/Node/DECIMAL.h"
#include "dupr/Ast/Node/ESCAPE_CHARS.h"

#include "dupr/Ast/Node/program.h"
#include "dupr/Ast/Node/deamerreserved_star__stmt__.h"
#include "dupr/Ast/Node/stmt.h"
#include "dupr/Ast/Node/pattern_execution.h"
#include "dupr/Ast/Node/pattern_constructor_array.h"
#include "dupr/Ast/Node/deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____.h"
#include "dupr/Ast/Node/pattern_constructor.h"
#include "dupr/Ast/Node/pattern_type.h"
#include "dupr/Ast/Node/pattern_name.h"
#include "dupr/Ast/Node/deamerreserved_arrow__VARNAME__.h"
#include "dupr/Ast/Node/deamerreserved_star__GT__VARNAME__.h"
#include "dupr/Ast/Node/pattern_constructor_content.h"
#include "dupr/Ast/Node/deamerreserved_plus__pattern_constructor_content_stmt__.h"
#include "dupr/Ast/Node/pattern_constructor_content_stmt.h"
#include "dupr/Ast/Node/pattern_constructor_operator.h"
#include "dupr/Ast/Node/pattern_constructor_structure.h"
#include "dupr/Ast/Node/pattern_constructor_terminate.h"
#include "dupr/Ast/Node/pattern_constructor_encapsulation.h"
#include "Bison/dupr_parser.tab.h"
void showError();
extern int duprlex();

static bool local_store = false;
static int column = 0;
static int handleColumn(const std::string& text);
static void store(const deamer::external::cpp::lexer::TerminalObject* const newObject);
static std::vector<const deamer::external::cpp::lexer::TerminalObject*> local_objects;
#ifndef yyval
#define yyval yytext
#endif //yyval
%}

LEFT_BRACKET	([{])
RIGHT_BRACKET	([}])
LEFT_PARANTHESIS	([(])
RIGHT_PARANTHESIS	([)])
LEFT_SQUARE_BRACKET	([\[])
RIGHT_SQUARE_BRACKET	([\]])
ADD	([+])
MINUS	([-])
MULTI	([*])
DIVIDE	([/])
LT	([<])
LE	([<][=])
GT	([>])
GE	([>][=])
EQ	([=])
OR	([|])
AND	([&])
DOT	([.])
COMMA	([,])
COLON	([:])
SEMICOLON	([;])
SIGN	([$])
HEKJE	([#])
QUESTION	([?])
EXCLAM	([!])
PATTERN_INSERTION	(([\[][\[][a-zA-Z_]+[a-zA-Z_0-9]*[\]][\]])|([\[][\[][a-zA-Z_>]+[a-zA-Z_0-9>]*[:][a-zA-Z_]+[a-zA-Z_0-9]*[\]][\]]))
VARNAME	([a-zA-Z_]+[a-zA-Z_0-9]*)
NUMBER	([0-9]+)
DECIMAL	([0-9]+[.][0-9]+)
ESCAPE_CHARS	([\n\r\t ]+)

%%

{LEFT_BRACKET}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (LEFT_BRACKET);;}
{RIGHT_BRACKET}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (RIGHT_BRACKET);;}
{LEFT_PARANTHESIS}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (LEFT_PARANTHESIS);;}
{RIGHT_PARANTHESIS}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (RIGHT_PARANTHESIS);;}
{LEFT_SQUARE_BRACKET}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (LEFT_SQUARE_BRACKET);;}
{RIGHT_SQUARE_BRACKET}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (RIGHT_SQUARE_BRACKET);;}
{ADD}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (ADD);;}
{MINUS}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (MINUS);;}
{MULTI}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (MULTI);;}
{DIVIDE}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (DIVIDE);;}
{LT}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (LT);;}
{LE}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (LE);;}
{GT}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (GT);;}
{GE}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (GE);;}
{EQ}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (EQ);;}
{OR}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (OR);;}
{AND}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (AND);;}
{DOT}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (DOT);;}
{COMMA}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (COMMA);;}
{COLON}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (COLON);;}
{SEMICOLON}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (SEMICOLON);;}
{SIGN}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (SIGN);;}
{HEKJE}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (HEKJE);;}
{QUESTION}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (QUESTION);;}
{EXCLAM}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (EXCLAM);;}
{PATTERN_INSERTION}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (PATTERN_INSERTION);;}
{VARNAME}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (VARNAME);;}
{NUMBER}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (NUMBER);;}
{DECIMAL}	{if (local_store) store(new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column));else duprlval.Terminal = new deamer::external::cpp::lexer::TerminalObject(yyval, yylineno, column);;handleColumn(yyval);if (!local_store) return (DECIMAL);;}
{ESCAPE_CHARS}	{handleColumn(yyval);if (!local_store) ;}

%%
static void store(const deamer::external::cpp::lexer::TerminalObject* const newObject)
{
	local_objects.push_back(newObject);
}

static void clear()
{
	local_objects.clear();
}

static int handleColumn(const std::string& text)
{
	int currentColumn = column;
	currentColumn += text.size();
	for (auto character : text)
	{
		if (character == '\n')
		{
			currentColumn = 0;
			break;
		}
	}
	
	return currentColumn;
}

std::vector<const deamer::external::cpp::lexer::TerminalObject*> dupr::lexer::Lexer::Tokenize(const std::string& text) const
{
	local_store = true;
	column = 0;

	YY_BUFFER_STATE buf;
	buf = yy_scan_string(text.c_str());
	yylex();
	yy_delete_buffer(buf);
	yylex_destroy();

	local_store = false;

	auto local_objects_copy = local_objects;
	clear();

	return local_objects_copy;
}


