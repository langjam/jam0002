#include "dupr/Grammar.h"
#include "dupr/Language.h"

dupr::Grammar::Grammar(dupr::Language* language)
			:	::deamer::language::generator::definition::property::user::Grammar<
					dupr::Language>(language)
{
}

void dupr::Grammar::GenerateObjects()
{
	// Non-Terminal initialization
	program.Set(::deamer::language::type::definition::object::main::NonTerminal("program", { program_deamerreserved_star__stmt__.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
deamerreserved_star__stmt__.Set(::deamer::language::type::definition::object::main::NonTerminal("deamerreserved_star__stmt__", { deamerreserved_star__stmt___stmt_deamerreserved_star__stmt__.Pointer(),deamerreserved_star__stmt___EMPTY.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , true));
stmt.Set(::deamer::language::type::definition::object::main::NonTerminal("stmt", { stmt_pattern_constructor.Pointer(),stmt_pattern_execution.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
pattern_execution.Set(::deamer::language::type::definition::object::main::NonTerminal("pattern_execution", { pattern_execution_pattern_name_LEFT_BRACKET_pattern_execution_content_RIGHT_BRACKET.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
pattern_execution_content.Set(::deamer::language::type::definition::object::main::NonTerminal("pattern_execution_content", { pattern_execution_content_deamerreserved_plus__pattern_execution_content_stmt__.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
deamerreserved_plus__pattern_execution_content_stmt__.Set(::deamer::language::type::definition::object::main::NonTerminal("deamerreserved_plus__pattern_execution_content_stmt__", { deamerreserved_plus__pattern_execution_content_stmt___pattern_execution_content_stmt.Pointer(),deamerreserved_plus__pattern_execution_content_stmt___pattern_execution_content_stmt_deamerreserved_plus__pattern_execution_content_stmt__.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , true));
pattern_execution_content_stmt.Set(::deamer::language::type::definition::object::main::NonTerminal("pattern_execution_content_stmt", { pattern_execution_content_stmt_pattern_constructor_content_stmt.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
pattern_constructor.Set(::deamer::language::type::definition::object::main::NonTerminal("pattern_constructor", { pattern_constructor_pattern_type_COLON_pattern_name_LEFT_BRACKET_pattern_constructor_content_RIGHT_BRACKET.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
pattern_type.Set(::deamer::language::type::definition::object::main::NonTerminal("pattern_type", { pattern_type_VARNAME.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
pattern_name.Set(::deamer::language::type::definition::object::main::NonTerminal("pattern_name", { pattern_name_VARNAME.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
pattern_constructor_content.Set(::deamer::language::type::definition::object::main::NonTerminal("pattern_constructor_content", { pattern_constructor_content_deamerreserved_plus__pattern_constructor_content_stmt__.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
deamerreserved_plus__pattern_constructor_content_stmt__.Set(::deamer::language::type::definition::object::main::NonTerminal("deamerreserved_plus__pattern_constructor_content_stmt__", { deamerreserved_plus__pattern_constructor_content_stmt___pattern_constructor_content_stmt.Pointer(),deamerreserved_plus__pattern_constructor_content_stmt___pattern_constructor_content_stmt_deamerreserved_plus__pattern_constructor_content_stmt__.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , true));
pattern_constructor_content_stmt.Set(::deamer::language::type::definition::object::main::NonTerminal("pattern_constructor_content_stmt", { pattern_constructor_content_stmt_pattern_constructor_operator.Pointer(),pattern_constructor_content_stmt_pattern_constructor_terminate.Pointer(),pattern_constructor_content_stmt_pattern_constructor_encapsulation.Pointer(),pattern_constructor_content_stmt_pattern_constructor_structure.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
pattern_constructor_operator.Set(::deamer::language::type::definition::object::main::NonTerminal("pattern_constructor_operator", { pattern_constructor_operator_ADD.Pointer(),pattern_constructor_operator_MINUS.Pointer(),pattern_constructor_operator_MULTI.Pointer(),pattern_constructor_operator_DIVIDE.Pointer(),pattern_constructor_operator_LT.Pointer(),pattern_constructor_operator_LE.Pointer(),pattern_constructor_operator_GT.Pointer(),pattern_constructor_operator_GE.Pointer(),pattern_constructor_operator_EQ.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
pattern_constructor_structure.Set(::deamer::language::type::definition::object::main::NonTerminal("pattern_constructor_structure", { pattern_constructor_structure_DOT.Pointer(),pattern_constructor_structure_COMMA.Pointer(),pattern_constructor_structure_COLON.Pointer(),pattern_constructor_structure_SEMICOLON.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
pattern_constructor_terminate.Set(::deamer::language::type::definition::object::main::NonTerminal("pattern_constructor_terminate", { pattern_constructor_terminate_PATTERN_INSERTION.Pointer(),pattern_constructor_terminate_VARNAME.Pointer(),pattern_constructor_terminate_NUMBER.Pointer(),pattern_constructor_terminate_DECIMAL.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
pattern_constructor_encapsulation.Set(::deamer::language::type::definition::object::main::NonTerminal("pattern_constructor_encapsulation", { pattern_constructor_encapsulation_LEFT_BRACKET_deamerreserved_plus__pattern_constructor_content_stmt___RIGHT_BRACKET.Pointer(),pattern_constructor_encapsulation_LEFT_PARANTHESIS_deamerreserved_plus__pattern_constructor_content_stmt___RIGHT_PARANTHESIS.Pointer(),pattern_constructor_encapsulation_LEFT_SQUARE_BRACKET_deamerreserved_plus__pattern_constructor_content_stmt___RIGHT_SQUARE_BRACKET.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));


	// Production-Rule initialization
	deamerreserved_star__stmt___stmt_deamerreserved_star__stmt__.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->stmt.Pointer(),Language->deamerreserved_star__stmt__.Pointer() }));
deamerreserved_star__stmt___EMPTY.Set(::deamer::language::type::definition::object::main::ProductionRule());
program_deamerreserved_star__stmt__.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->deamerreserved_star__stmt__.Pointer() }));
stmt_pattern_constructor.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_constructor.Pointer() }));
stmt_pattern_execution.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_execution.Pointer() }));
pattern_execution_pattern_name_LEFT_BRACKET_pattern_execution_content_RIGHT_BRACKET.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_name.Pointer(),Language->LEFT_BRACKET.Pointer(),Language->pattern_execution_content.Pointer(),Language->RIGHT_BRACKET.Pointer() }));
deamerreserved_plus__pattern_execution_content_stmt___pattern_execution_content_stmt.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_execution_content_stmt.Pointer() }));
deamerreserved_plus__pattern_execution_content_stmt___pattern_execution_content_stmt_deamerreserved_plus__pattern_execution_content_stmt__.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_execution_content_stmt.Pointer(),Language->deamerreserved_plus__pattern_execution_content_stmt__.Pointer() }));
pattern_execution_content_deamerreserved_plus__pattern_execution_content_stmt__.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->deamerreserved_plus__pattern_execution_content_stmt__.Pointer() }));
pattern_execution_content_stmt_pattern_constructor_content_stmt.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_constructor_content_stmt.Pointer() }));
pattern_constructor_pattern_type_COLON_pattern_name_LEFT_BRACKET_pattern_constructor_content_RIGHT_BRACKET.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_type.Pointer(),Language->COLON.Pointer(),Language->pattern_name.Pointer(),Language->LEFT_BRACKET.Pointer(),Language->pattern_constructor_content.Pointer(),Language->RIGHT_BRACKET.Pointer() }));
pattern_type_VARNAME.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->VARNAME.Pointer() }));
pattern_name_VARNAME.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->VARNAME.Pointer() }));
deamerreserved_plus__pattern_constructor_content_stmt___pattern_constructor_content_stmt.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_constructor_content_stmt.Pointer() }));
deamerreserved_plus__pattern_constructor_content_stmt___pattern_constructor_content_stmt_deamerreserved_plus__pattern_constructor_content_stmt__.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_constructor_content_stmt.Pointer(),Language->deamerreserved_plus__pattern_constructor_content_stmt__.Pointer() }));
pattern_constructor_content_deamerreserved_plus__pattern_constructor_content_stmt__.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->deamerreserved_plus__pattern_constructor_content_stmt__.Pointer() }));
pattern_constructor_content_stmt_pattern_constructor_operator.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_constructor_operator.Pointer() }));
pattern_constructor_content_stmt_pattern_constructor_terminate.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_constructor_terminate.Pointer() }));
pattern_constructor_content_stmt_pattern_constructor_encapsulation.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_constructor_encapsulation.Pointer() }));
pattern_constructor_content_stmt_pattern_constructor_structure.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_constructor_structure.Pointer() }));
pattern_constructor_operator_ADD.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->ADD.Pointer() }));
pattern_constructor_operator_MINUS.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->MINUS.Pointer() }));
pattern_constructor_operator_MULTI.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->MULTI.Pointer() }));
pattern_constructor_operator_DIVIDE.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->DIVIDE.Pointer() }));
pattern_constructor_operator_LT.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->LT.Pointer() }));
pattern_constructor_operator_LE.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->LE.Pointer() }));
pattern_constructor_operator_GT.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->GT.Pointer() }));
pattern_constructor_operator_GE.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->GE.Pointer() }));
pattern_constructor_operator_EQ.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->EQ.Pointer() }));
pattern_constructor_structure_DOT.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->DOT.Pointer() }));
pattern_constructor_structure_COMMA.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->COMMA.Pointer() }));
pattern_constructor_structure_COLON.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->COLON.Pointer() }));
pattern_constructor_structure_SEMICOLON.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->SEMICOLON.Pointer() }));
pattern_constructor_terminate_PATTERN_INSERTION.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->PATTERN_INSERTION.Pointer() }));
pattern_constructor_terminate_VARNAME.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->VARNAME.Pointer() }));
pattern_constructor_terminate_NUMBER.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->NUMBER.Pointer() }));
pattern_constructor_terminate_DECIMAL.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->DECIMAL.Pointer() }));
pattern_constructor_encapsulation_LEFT_BRACKET_deamerreserved_plus__pattern_constructor_content_stmt___RIGHT_BRACKET.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->LEFT_BRACKET.Pointer(),Language->deamerreserved_plus__pattern_constructor_content_stmt__.Pointer(),Language->RIGHT_BRACKET.Pointer() }));
pattern_constructor_encapsulation_LEFT_PARANTHESIS_deamerreserved_plus__pattern_constructor_content_stmt___RIGHT_PARANTHESIS.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->LEFT_PARANTHESIS.Pointer(),Language->deamerreserved_plus__pattern_constructor_content_stmt__.Pointer(),Language->RIGHT_PARANTHESIS.Pointer() }));
pattern_constructor_encapsulation_LEFT_SQUARE_BRACKET_deamerreserved_plus__pattern_constructor_content_stmt___RIGHT_SQUARE_BRACKET.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->LEFT_SQUARE_BRACKET.Pointer(),Language->deamerreserved_plus__pattern_constructor_content_stmt__.Pointer(),Language->RIGHT_SQUARE_BRACKET.Pointer() }));


	// Unknown references
	

	// Add object calls
	// AddObject(...)
	AddObject(program);
AddObject(deamerreserved_star__stmt__);
AddObject(stmt);
AddObject(pattern_execution);
AddObject(pattern_execution_content);
AddObject(deamerreserved_plus__pattern_execution_content_stmt__);
AddObject(pattern_execution_content_stmt);
AddObject(pattern_constructor);
AddObject(pattern_type);
AddObject(pattern_name);
AddObject(pattern_constructor_content);
AddObject(deamerreserved_plus__pattern_constructor_content_stmt__);
AddObject(pattern_constructor_content_stmt);
AddObject(pattern_constructor_operator);
AddObject(pattern_constructor_structure);
AddObject(pattern_constructor_terminate);
AddObject(pattern_constructor_encapsulation);


	AddObject(deamerreserved_star__stmt___stmt_deamerreserved_star__stmt__);
AddObject(deamerreserved_star__stmt___EMPTY);
AddObject(program_deamerreserved_star__stmt__);
AddObject(stmt_pattern_constructor);
AddObject(stmt_pattern_execution);
AddObject(pattern_execution_pattern_name_LEFT_BRACKET_pattern_execution_content_RIGHT_BRACKET);
AddObject(deamerreserved_plus__pattern_execution_content_stmt___pattern_execution_content_stmt);
AddObject(deamerreserved_plus__pattern_execution_content_stmt___pattern_execution_content_stmt_deamerreserved_plus__pattern_execution_content_stmt__);
AddObject(pattern_execution_content_deamerreserved_plus__pattern_execution_content_stmt__);
AddObject(pattern_execution_content_stmt_pattern_constructor_content_stmt);
AddObject(pattern_constructor_pattern_type_COLON_pattern_name_LEFT_BRACKET_pattern_constructor_content_RIGHT_BRACKET);
AddObject(pattern_type_VARNAME);
AddObject(pattern_name_VARNAME);
AddObject(deamerreserved_plus__pattern_constructor_content_stmt___pattern_constructor_content_stmt);
AddObject(deamerreserved_plus__pattern_constructor_content_stmt___pattern_constructor_content_stmt_deamerreserved_plus__pattern_constructor_content_stmt__);
AddObject(pattern_constructor_content_deamerreserved_plus__pattern_constructor_content_stmt__);
AddObject(pattern_constructor_content_stmt_pattern_constructor_operator);
AddObject(pattern_constructor_content_stmt_pattern_constructor_terminate);
AddObject(pattern_constructor_content_stmt_pattern_constructor_encapsulation);
AddObject(pattern_constructor_content_stmt_pattern_constructor_structure);
AddObject(pattern_constructor_operator_ADD);
AddObject(pattern_constructor_operator_MINUS);
AddObject(pattern_constructor_operator_MULTI);
AddObject(pattern_constructor_operator_DIVIDE);
AddObject(pattern_constructor_operator_LT);
AddObject(pattern_constructor_operator_LE);
AddObject(pattern_constructor_operator_GT);
AddObject(pattern_constructor_operator_GE);
AddObject(pattern_constructor_operator_EQ);
AddObject(pattern_constructor_structure_DOT);
AddObject(pattern_constructor_structure_COMMA);
AddObject(pattern_constructor_structure_COLON);
AddObject(pattern_constructor_structure_SEMICOLON);
AddObject(pattern_constructor_terminate_PATTERN_INSERTION);
AddObject(pattern_constructor_terminate_VARNAME);
AddObject(pattern_constructor_terminate_NUMBER);
AddObject(pattern_constructor_terminate_DECIMAL);
AddObject(pattern_constructor_encapsulation_LEFT_BRACKET_deamerreserved_plus__pattern_constructor_content_stmt___RIGHT_BRACKET);
AddObject(pattern_constructor_encapsulation_LEFT_PARANTHESIS_deamerreserved_plus__pattern_constructor_content_stmt___RIGHT_PARANTHESIS);
AddObject(pattern_constructor_encapsulation_LEFT_SQUARE_BRACKET_deamerreserved_plus__pattern_constructor_content_stmt___RIGHT_SQUARE_BRACKET);


	


	// Place higher level operations here.
	// ReplaceObject(..., ...)
	// DeleteObject(..., ...)
	
}
