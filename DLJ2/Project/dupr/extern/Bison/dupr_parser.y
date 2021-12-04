%{
#include <iostream>
#include <vector>
#include <cstring>
#include <stdio.h>
#include <Deamer/External/Cpp/Lexer/TerminalObject.h>
#include <Deamer/External/Cpp/Ast/Node.h>
#include "dupr/Bison/Parser.h"
#define YY_NO_UNISTD_H
#include "Flex/dupr_lexer.h"
#undef YY_NO_UNISTD_H
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

#ifndef YY_parse_NERRS
#define YY_parse_NERRS duprnerrs
#endif //YY_parse_NERRS
#ifndef YY_parse_LLOC
#define YY_parse_LLOC duprlloc
#endif //YY_parse_LLOC
#define YYERROR_VERBOSE

void duprerror(const char* s);
int duprlex();
static ::deamer::external::cpp::ast::Tree* outputTree = nullptr;
%}

%token<Terminal> LEFT_BRACKET
%token<Terminal> RIGHT_BRACKET
%token<Terminal> LEFT_PARANTHESIS
%token<Terminal> RIGHT_PARANTHESIS
%token<Terminal> LEFT_SQUARE_BRACKET
%token<Terminal> RIGHT_SQUARE_BRACKET
%token<Terminal> ADD
%token<Terminal> MINUS
%token<Terminal> MULTI
%token<Terminal> DIVIDE
%token<Terminal> LT
%token<Terminal> LE
%token<Terminal> GT
%token<Terminal> GE
%token<Terminal> EQ
%token<Terminal> OR
%token<Terminal> AND
%token<Terminal> DOT
%token<Terminal> COMMA
%token<Terminal> COLON
%token<Terminal> SEMICOLON
%token<Terminal> SIGN
%token<Terminal> HEKJE
%token<Terminal> QUESTION
%token<Terminal> EXCLAM
%token<Terminal> PATTERN_INSERTION
%token<Terminal> VARNAME
%token<Terminal> NUMBER
%token<Terminal> DECIMAL

%nterm<dupr_program> program
%nterm<dupr_deamerreserved_star__stmt__> deamerreserved_star__stmt__
%nterm<dupr_stmt> stmt
%nterm<dupr_pattern_execution> pattern_execution
%nterm<dupr_pattern_constructor_array> pattern_constructor_array
%nterm<dupr_deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____> deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____
%nterm<dupr_pattern_constructor> pattern_constructor
%nterm<dupr_pattern_type> pattern_type
%nterm<dupr_pattern_name> pattern_name
%nterm<dupr_deamerreserved_arrow__VARNAME__> deamerreserved_arrow__VARNAME__
%nterm<dupr_deamerreserved_star__GT__VARNAME__> deamerreserved_star__GT__VARNAME__
%nterm<dupr_pattern_constructor_content> pattern_constructor_content
%nterm<dupr_deamerreserved_plus__pattern_constructor_content_stmt__> deamerreserved_plus__pattern_constructor_content_stmt__
%nterm<dupr_pattern_constructor_content_stmt> pattern_constructor_content_stmt
%nterm<dupr_pattern_constructor_operator> pattern_constructor_operator
%nterm<dupr_pattern_constructor_structure> pattern_constructor_structure
%nterm<dupr_pattern_constructor_terminate> pattern_constructor_terminate
%nterm<dupr_pattern_constructor_encapsulation> pattern_constructor_encapsulation



%union{
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
	::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__* dupr_deamerreserved_plus__pattern_constructor_content_stmt__;
	::dupr::ast::node::pattern_constructor_content_stmt* dupr_pattern_constructor_content_stmt;
	::dupr::ast::node::pattern_constructor_operator* dupr_pattern_constructor_operator;
	::dupr::ast::node::pattern_constructor_structure* dupr_pattern_constructor_structure;
	::dupr::ast::node::pattern_constructor_terminate* dupr_pattern_constructor_terminate;
	::dupr::ast::node::pattern_constructor_encapsulation* dupr_pattern_constructor_encapsulation;
}

%%

program:
	deamerreserved_star__stmt__ {
		auto* const newNode = new dupr::ast::node::program({::dupr::ast::Type::program, ::deamer::external::cpp::ast::NodeValue::nonterminal, {0, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { $1 });
		$$ = newNode;
		outputTree = new ::deamer::external::cpp::ast::Tree(newNode);
	}
;

deamerreserved_star__stmt__:
	stmt deamerreserved_star__stmt__ {
		auto* const newNode = new dupr::ast::node::deamerreserved_star__stmt__({::dupr::ast::Type::deamerreserved_star__stmt__, ::deamer::external::cpp::ast::NodeValue::nonterminal, {0, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { $1, $2 });
		$$ = newNode;
	}
	| {
		auto* const newNode = new dupr::ast::node::deamerreserved_star__stmt__({::dupr::ast::Type::deamerreserved_star__stmt__, ::deamer::external::cpp::ast::NodeValue::nonterminal, {1, ::deamer::external::cpp::ast::ProductionRuleType::user}}, {  });
		$$ = newNode;
	}
;

stmt:
	pattern_constructor {
		auto* const newNode = new dupr::ast::node::stmt({::dupr::ast::Type::stmt, ::deamer::external::cpp::ast::NodeValue::nonterminal, {0, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { $1 });
		$$ = newNode;
	}
	| pattern_constructor_array {
		auto* const newNode = new dupr::ast::node::stmt({::dupr::ast::Type::stmt, ::deamer::external::cpp::ast::NodeValue::nonterminal, {1, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { $1 });
		$$ = newNode;
	}
	| pattern_execution {
		auto* const newNode = new dupr::ast::node::stmt({::dupr::ast::Type::stmt, ::deamer::external::cpp::ast::NodeValue::nonterminal, {2, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { $1 });
		$$ = newNode;
	}
;

pattern_execution:
	pattern_name LEFT_BRACKET pattern_constructor_content RIGHT_BRACKET {
		auto* const newNode = new dupr::ast::node::pattern_execution({::dupr::ast::Type::pattern_execution, ::deamer::external::cpp::ast::NodeValue::nonterminal, {0, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { $1, new dupr::ast::node::LEFT_BRACKET({::dupr::ast::Type::LEFT_BRACKET, ::deamer::external::cpp::ast::NodeValue::terminal, $2}), $3, new dupr::ast::node::RIGHT_BRACKET({::dupr::ast::Type::RIGHT_BRACKET, ::deamer::external::cpp::ast::NodeValue::terminal, $4}) });
		$$ = newNode;
	}
;

pattern_constructor_array:
	pattern_type COLON pattern_name LEFT_SQUARE_BRACKET deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____ RIGHT_SQUARE_BRACKET {
		auto* const newNode = new dupr::ast::node::pattern_constructor_array({::dupr::ast::Type::pattern_constructor_array, ::deamer::external::cpp::ast::NodeValue::nonterminal, {0, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { $1, new dupr::ast::node::COLON({::dupr::ast::Type::COLON, ::deamer::external::cpp::ast::NodeValue::terminal, $2}), $3, new dupr::ast::node::LEFT_SQUARE_BRACKET({::dupr::ast::Type::LEFT_SQUARE_BRACKET, ::deamer::external::cpp::ast::NodeValue::terminal, $4}), $5, new dupr::ast::node::RIGHT_SQUARE_BRACKET({::dupr::ast::Type::RIGHT_SQUARE_BRACKET, ::deamer::external::cpp::ast::NodeValue::terminal, $6}) });
		$$ = newNode;
	}
;

deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____:
	pattern_constructor {
		auto* const newNode = new dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____({::dupr::ast::Type::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____, ::deamer::external::cpp::ast::NodeValue::nonterminal, {0, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { $1 });
		$$ = newNode;
	}
	| pattern_constructor_array {
		auto* const newNode = new dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____({::dupr::ast::Type::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____, ::deamer::external::cpp::ast::NodeValue::nonterminal, {1, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { $1 });
		$$ = newNode;
	}
	| pattern_constructor deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____ {
		auto* const newNode = new dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____({::dupr::ast::Type::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____, ::deamer::external::cpp::ast::NodeValue::nonterminal, {2, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { $1, $2 });
		$$ = newNode;
	}
	| pattern_constructor_array deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____ {
		auto* const newNode = new dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____({::dupr::ast::Type::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____, ::deamer::external::cpp::ast::NodeValue::nonterminal, {3, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { $1, $2 });
		$$ = newNode;
	}
;

pattern_constructor:
	pattern_type COLON pattern_name LEFT_BRACKET pattern_constructor_content RIGHT_BRACKET {
		auto* const newNode = new dupr::ast::node::pattern_constructor({::dupr::ast::Type::pattern_constructor, ::deamer::external::cpp::ast::NodeValue::nonterminal, {0, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { $1, new dupr::ast::node::COLON({::dupr::ast::Type::COLON, ::deamer::external::cpp::ast::NodeValue::terminal, $2}), $3, new dupr::ast::node::LEFT_BRACKET({::dupr::ast::Type::LEFT_BRACKET, ::deamer::external::cpp::ast::NodeValue::terminal, $4}), $5, new dupr::ast::node::RIGHT_BRACKET({::dupr::ast::Type::RIGHT_BRACKET, ::deamer::external::cpp::ast::NodeValue::terminal, $6}) });
		$$ = newNode;
	}
;

pattern_type:
	VARNAME {
		auto* const newNode = new dupr::ast::node::pattern_type({::dupr::ast::Type::pattern_type, ::deamer::external::cpp::ast::NodeValue::nonterminal, {0, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::VARNAME({::dupr::ast::Type::VARNAME, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
;

pattern_name:
	deamerreserved_arrow__VARNAME__ {
		auto* const newNode = new dupr::ast::node::pattern_name({::dupr::ast::Type::pattern_name, ::deamer::external::cpp::ast::NodeValue::nonterminal, {0, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { $1 });
		$$ = newNode;
	}
;

deamerreserved_arrow__VARNAME__:
	VARNAME deamerreserved_star__GT__VARNAME__ {
		auto* const newNode = new dupr::ast::node::deamerreserved_arrow__VARNAME__({::dupr::ast::Type::deamerreserved_arrow__VARNAME__, ::deamer::external::cpp::ast::NodeValue::nonterminal, {0, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::VARNAME({::dupr::ast::Type::VARNAME, ::deamer::external::cpp::ast::NodeValue::terminal, $1}), $2 });
		$$ = newNode;
	}
;

deamerreserved_star__GT__VARNAME__:
	GT VARNAME deamerreserved_star__GT__VARNAME__ {
		auto* const newNode = new dupr::ast::node::deamerreserved_star__GT__VARNAME__({::dupr::ast::Type::deamerreserved_star__GT__VARNAME__, ::deamer::external::cpp::ast::NodeValue::nonterminal, {0, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::GT({::dupr::ast::Type::GT, ::deamer::external::cpp::ast::NodeValue::terminal, $1}), new dupr::ast::node::VARNAME({::dupr::ast::Type::VARNAME, ::deamer::external::cpp::ast::NodeValue::terminal, $2}), $3 });
		$$ = newNode;
	}
	| {
		auto* const newNode = new dupr::ast::node::deamerreserved_star__GT__VARNAME__({::dupr::ast::Type::deamerreserved_star__GT__VARNAME__, ::deamer::external::cpp::ast::NodeValue::nonterminal, {1, ::deamer::external::cpp::ast::ProductionRuleType::user}}, {  });
		$$ = newNode;
	}
;

pattern_constructor_content:
	deamerreserved_plus__pattern_constructor_content_stmt__ {
		auto* const newNode = new dupr::ast::node::pattern_constructor_content({::dupr::ast::Type::pattern_constructor_content, ::deamer::external::cpp::ast::NodeValue::nonterminal, {0, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { $1 });
		$$ = newNode;
	}
;

deamerreserved_plus__pattern_constructor_content_stmt__:
	pattern_constructor_content_stmt {
		auto* const newNode = new dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__({::dupr::ast::Type::deamerreserved_plus__pattern_constructor_content_stmt__, ::deamer::external::cpp::ast::NodeValue::nonterminal, {0, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { $1 });
		$$ = newNode;
	}
	| pattern_constructor_content_stmt deamerreserved_plus__pattern_constructor_content_stmt__ {
		auto* const newNode = new dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__({::dupr::ast::Type::deamerreserved_plus__pattern_constructor_content_stmt__, ::deamer::external::cpp::ast::NodeValue::nonterminal, {1, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { $1, $2 });
		$$ = newNode;
	}
;

pattern_constructor_content_stmt:
	pattern_constructor_operator {
		auto* const newNode = new dupr::ast::node::pattern_constructor_content_stmt({::dupr::ast::Type::pattern_constructor_content_stmt, ::deamer::external::cpp::ast::NodeValue::nonterminal, {0, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { $1 });
		$$ = newNode;
	}
	| pattern_constructor_terminate {
		auto* const newNode = new dupr::ast::node::pattern_constructor_content_stmt({::dupr::ast::Type::pattern_constructor_content_stmt, ::deamer::external::cpp::ast::NodeValue::nonterminal, {1, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { $1 });
		$$ = newNode;
	}
	| pattern_constructor_encapsulation {
		auto* const newNode = new dupr::ast::node::pattern_constructor_content_stmt({::dupr::ast::Type::pattern_constructor_content_stmt, ::deamer::external::cpp::ast::NodeValue::nonterminal, {2, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { $1 });
		$$ = newNode;
	}
	| pattern_constructor_structure {
		auto* const newNode = new dupr::ast::node::pattern_constructor_content_stmt({::dupr::ast::Type::pattern_constructor_content_stmt, ::deamer::external::cpp::ast::NodeValue::nonterminal, {3, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { $1 });
		$$ = newNode;
	}
;

pattern_constructor_operator:
	ADD {
		auto* const newNode = new dupr::ast::node::pattern_constructor_operator({::dupr::ast::Type::pattern_constructor_operator, ::deamer::external::cpp::ast::NodeValue::nonterminal, {0, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::ADD({::dupr::ast::Type::ADD, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
	| MINUS {
		auto* const newNode = new dupr::ast::node::pattern_constructor_operator({::dupr::ast::Type::pattern_constructor_operator, ::deamer::external::cpp::ast::NodeValue::nonterminal, {1, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::MINUS({::dupr::ast::Type::MINUS, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
	| MULTI {
		auto* const newNode = new dupr::ast::node::pattern_constructor_operator({::dupr::ast::Type::pattern_constructor_operator, ::deamer::external::cpp::ast::NodeValue::nonterminal, {2, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::MULTI({::dupr::ast::Type::MULTI, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
	| DIVIDE {
		auto* const newNode = new dupr::ast::node::pattern_constructor_operator({::dupr::ast::Type::pattern_constructor_operator, ::deamer::external::cpp::ast::NodeValue::nonterminal, {3, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::DIVIDE({::dupr::ast::Type::DIVIDE, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
	| LT {
		auto* const newNode = new dupr::ast::node::pattern_constructor_operator({::dupr::ast::Type::pattern_constructor_operator, ::deamer::external::cpp::ast::NodeValue::nonterminal, {4, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::LT({::dupr::ast::Type::LT, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
	| LE {
		auto* const newNode = new dupr::ast::node::pattern_constructor_operator({::dupr::ast::Type::pattern_constructor_operator, ::deamer::external::cpp::ast::NodeValue::nonterminal, {5, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::LE({::dupr::ast::Type::LE, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
	| GT {
		auto* const newNode = new dupr::ast::node::pattern_constructor_operator({::dupr::ast::Type::pattern_constructor_operator, ::deamer::external::cpp::ast::NodeValue::nonterminal, {6, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::GT({::dupr::ast::Type::GT, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
	| GE {
		auto* const newNode = new dupr::ast::node::pattern_constructor_operator({::dupr::ast::Type::pattern_constructor_operator, ::deamer::external::cpp::ast::NodeValue::nonterminal, {7, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::GE({::dupr::ast::Type::GE, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
	| EQ {
		auto* const newNode = new dupr::ast::node::pattern_constructor_operator({::dupr::ast::Type::pattern_constructor_operator, ::deamer::external::cpp::ast::NodeValue::nonterminal, {8, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::EQ({::dupr::ast::Type::EQ, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
	| AND {
		auto* const newNode = new dupr::ast::node::pattern_constructor_operator({::dupr::ast::Type::pattern_constructor_operator, ::deamer::external::cpp::ast::NodeValue::nonterminal, {9, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::AND({::dupr::ast::Type::AND, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
	| OR {
		auto* const newNode = new dupr::ast::node::pattern_constructor_operator({::dupr::ast::Type::pattern_constructor_operator, ::deamer::external::cpp::ast::NodeValue::nonterminal, {10, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::OR({::dupr::ast::Type::OR, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
;

pattern_constructor_structure:
	DOT {
		auto* const newNode = new dupr::ast::node::pattern_constructor_structure({::dupr::ast::Type::pattern_constructor_structure, ::deamer::external::cpp::ast::NodeValue::nonterminal, {0, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::DOT({::dupr::ast::Type::DOT, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
	| COMMA {
		auto* const newNode = new dupr::ast::node::pattern_constructor_structure({::dupr::ast::Type::pattern_constructor_structure, ::deamer::external::cpp::ast::NodeValue::nonterminal, {1, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::COMMA({::dupr::ast::Type::COMMA, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
	| COLON {
		auto* const newNode = new dupr::ast::node::pattern_constructor_structure({::dupr::ast::Type::pattern_constructor_structure, ::deamer::external::cpp::ast::NodeValue::nonterminal, {2, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::COLON({::dupr::ast::Type::COLON, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
	| SEMICOLON {
		auto* const newNode = new dupr::ast::node::pattern_constructor_structure({::dupr::ast::Type::pattern_constructor_structure, ::deamer::external::cpp::ast::NodeValue::nonterminal, {3, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::SEMICOLON({::dupr::ast::Type::SEMICOLON, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
	| SIGN {
		auto* const newNode = new dupr::ast::node::pattern_constructor_structure({::dupr::ast::Type::pattern_constructor_structure, ::deamer::external::cpp::ast::NodeValue::nonterminal, {4, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::SIGN({::dupr::ast::Type::SIGN, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
	| HEKJE {
		auto* const newNode = new dupr::ast::node::pattern_constructor_structure({::dupr::ast::Type::pattern_constructor_structure, ::deamer::external::cpp::ast::NodeValue::nonterminal, {5, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::HEKJE({::dupr::ast::Type::HEKJE, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
	| QUESTION {
		auto* const newNode = new dupr::ast::node::pattern_constructor_structure({::dupr::ast::Type::pattern_constructor_structure, ::deamer::external::cpp::ast::NodeValue::nonterminal, {6, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::QUESTION({::dupr::ast::Type::QUESTION, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
	| EXCLAM {
		auto* const newNode = new dupr::ast::node::pattern_constructor_structure({::dupr::ast::Type::pattern_constructor_structure, ::deamer::external::cpp::ast::NodeValue::nonterminal, {7, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::EXCLAM({::dupr::ast::Type::EXCLAM, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
;

pattern_constructor_terminate:
	PATTERN_INSERTION {
		auto* const newNode = new dupr::ast::node::pattern_constructor_terminate({::dupr::ast::Type::pattern_constructor_terminate, ::deamer::external::cpp::ast::NodeValue::nonterminal, {0, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::PATTERN_INSERTION({::dupr::ast::Type::PATTERN_INSERTION, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
	| VARNAME {
		auto* const newNode = new dupr::ast::node::pattern_constructor_terminate({::dupr::ast::Type::pattern_constructor_terminate, ::deamer::external::cpp::ast::NodeValue::nonterminal, {1, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::VARNAME({::dupr::ast::Type::VARNAME, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
	| NUMBER {
		auto* const newNode = new dupr::ast::node::pattern_constructor_terminate({::dupr::ast::Type::pattern_constructor_terminate, ::deamer::external::cpp::ast::NodeValue::nonterminal, {2, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::NUMBER({::dupr::ast::Type::NUMBER, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
	| DECIMAL {
		auto* const newNode = new dupr::ast::node::pattern_constructor_terminate({::dupr::ast::Type::pattern_constructor_terminate, ::deamer::external::cpp::ast::NodeValue::nonterminal, {3, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::DECIMAL({::dupr::ast::Type::DECIMAL, ::deamer::external::cpp::ast::NodeValue::terminal, $1}) });
		$$ = newNode;
	}
;

pattern_constructor_encapsulation:
	LEFT_BRACKET pattern_constructor_content RIGHT_BRACKET {
		auto* const newNode = new dupr::ast::node::pattern_constructor_encapsulation({::dupr::ast::Type::pattern_constructor_encapsulation, ::deamer::external::cpp::ast::NodeValue::nonterminal, {0, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::LEFT_BRACKET({::dupr::ast::Type::LEFT_BRACKET, ::deamer::external::cpp::ast::NodeValue::terminal, $1}), $2, new dupr::ast::node::RIGHT_BRACKET({::dupr::ast::Type::RIGHT_BRACKET, ::deamer::external::cpp::ast::NodeValue::terminal, $3}) });
		$$ = newNode;
	}
	| LEFT_PARANTHESIS pattern_constructor_content RIGHT_PARANTHESIS {
		auto* const newNode = new dupr::ast::node::pattern_constructor_encapsulation({::dupr::ast::Type::pattern_constructor_encapsulation, ::deamer::external::cpp::ast::NodeValue::nonterminal, {1, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::LEFT_PARANTHESIS({::dupr::ast::Type::LEFT_PARANTHESIS, ::deamer::external::cpp::ast::NodeValue::terminal, $1}), $2, new dupr::ast::node::RIGHT_PARANTHESIS({::dupr::ast::Type::RIGHT_PARANTHESIS, ::deamer::external::cpp::ast::NodeValue::terminal, $3}) });
		$$ = newNode;
	}
	| LEFT_SQUARE_BRACKET pattern_constructor_content RIGHT_SQUARE_BRACKET {
		auto* const newNode = new dupr::ast::node::pattern_constructor_encapsulation({::dupr::ast::Type::pattern_constructor_encapsulation, ::deamer::external::cpp::ast::NodeValue::nonterminal, {2, ::deamer::external::cpp::ast::ProductionRuleType::user}}, { new dupr::ast::node::LEFT_SQUARE_BRACKET({::dupr::ast::Type::LEFT_SQUARE_BRACKET, ::deamer::external::cpp::ast::NodeValue::terminal, $1}), $2, new dupr::ast::node::RIGHT_SQUARE_BRACKET({::dupr::ast::Type::RIGHT_SQUARE_BRACKET, ::deamer::external::cpp::ast::NodeValue::terminal, $3}) });
		$$ = newNode;
	}
;

%%

void duprerror(const char* s)
{
	std::cout << "Syntax error on line: " << s << '\n';
}

deamer::external::cpp::ast::Tree* dupr::parser::Parser::Parse(const std::string& text) const
{
	outputTree = nullptr;
	YY_BUFFER_STATE buf;
	buf = dupr_scan_string(text.c_str());
	duprparse();
	dupr_delete_buffer(buf);
	duprlex_destroy();

	return outputTree;
}

