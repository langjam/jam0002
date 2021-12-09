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
stmt.Set(::deamer::language::type::definition::object::main::NonTerminal("stmt", { stmt_pattern_constructor.Pointer(),stmt_pattern_constructor_array.Pointer(),stmt_pattern_execution.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
pattern_execution.Set(::deamer::language::type::definition::object::main::NonTerminal("pattern_execution", { pattern_execution_pattern_name_LEFT_BRACKET_pattern_constructor_content_RIGHT_BRACKET.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
pattern_constructor_array.Set(::deamer::language::type::definition::object::main::NonTerminal("pattern_constructor_array", { pattern_constructor_array_pattern_type_COLON_pattern_name_LEFT_SQUARE_BRACKET_deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array_____RIGHT_SQUARE_BRACKET.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____.Set(::deamer::language::type::definition::object::main::NonTerminal("deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____", { deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array_____pattern_constructor.Pointer(),deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array_____pattern_constructor_array.Pointer(),deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array_____pattern_constructor_deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____.Pointer(),deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array_____pattern_constructor_array_deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , true));
pattern_constructor.Set(::deamer::language::type::definition::object::main::NonTerminal("pattern_constructor", { pattern_constructor_pattern_type_COLON_pattern_name_LEFT_BRACKET_pattern_constructor_content_RIGHT_BRACKET.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
pattern_type.Set(::deamer::language::type::definition::object::main::NonTerminal("pattern_type", { pattern_type_VARNAME.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
pattern_name.Set(::deamer::language::type::definition::object::main::NonTerminal("pattern_name", { pattern_name_deamerreserved_arrow__VARNAME__.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
deamerreserved_arrow__VARNAME__.Set(::deamer::language::type::definition::object::main::NonTerminal("deamerreserved_arrow__VARNAME__", { deamerreserved_arrow__VARNAME___VARNAME_deamerreserved_star__GT__VARNAME__.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , true));
deamerreserved_star__GT__VARNAME__.Set(::deamer::language::type::definition::object::main::NonTerminal("deamerreserved_star__GT__VARNAME__", { deamerreserved_star__GT__VARNAME___GT_VARNAME_deamerreserved_star__GT__VARNAME__.Pointer(),deamerreserved_star__GT__VARNAME___EMPTY.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , true));
pattern_constructor_content.Set(::deamer::language::type::definition::object::main::NonTerminal("pattern_constructor_content", { pattern_constructor_content_deamerreserved_star__pattern_constructor_content_stmt__.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
deamerreserved_star__pattern_constructor_content_stmt__.Set(::deamer::language::type::definition::object::main::NonTerminal("deamerreserved_star__pattern_constructor_content_stmt__", { deamerreserved_star__pattern_constructor_content_stmt___pattern_constructor_content_stmt_deamerreserved_star__pattern_constructor_content_stmt__.Pointer(),deamerreserved_star__pattern_constructor_content_stmt___EMPTY.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , true));
pattern_constructor_content_stmt.Set(::deamer::language::type::definition::object::main::NonTerminal("pattern_constructor_content_stmt", { pattern_constructor_content_stmt_pattern_constructor_operator.Pointer(),pattern_constructor_content_stmt_pattern_constructor_terminate.Pointer(),pattern_constructor_content_stmt_pattern_constructor_encapsulation.Pointer(),pattern_constructor_content_stmt_pattern_constructor_structure.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
pattern_constructor_operator.Set(::deamer::language::type::definition::object::main::NonTerminal("pattern_constructor_operator", { pattern_constructor_operator_ADD.Pointer(),pattern_constructor_operator_MINUS.Pointer(),pattern_constructor_operator_MULTI.Pointer(),pattern_constructor_operator_DIVIDE.Pointer(),pattern_constructor_operator_LT.Pointer(),pattern_constructor_operator_LE.Pointer(),pattern_constructor_operator_GT.Pointer(),pattern_constructor_operator_GE.Pointer(),pattern_constructor_operator_EQ.Pointer(),pattern_constructor_operator_AND.Pointer(),pattern_constructor_operator_OR.Pointer(),pattern_constructor_operator_EQEQ.Pointer(),pattern_constructor_operator_EQEQEQ.Pointer(),pattern_constructor_operator_OROR.Pointer(),pattern_constructor_operator_ANDAND.Pointer(),pattern_constructor_operator_WILDCARD_OP.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
pattern_constructor_structure.Set(::deamer::language::type::definition::object::main::NonTerminal("pattern_constructor_structure", { pattern_constructor_structure_DOT.Pointer(),pattern_constructor_structure_COMMA.Pointer(),pattern_constructor_structure_COLON.Pointer(),pattern_constructor_structure_SEMICOLON.Pointer(),pattern_constructor_structure_SIGN.Pointer(),pattern_constructor_structure_HEKJE.Pointer(),pattern_constructor_structure_QUESTION.Pointer(),pattern_constructor_structure_EXCLAM.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
pattern_constructor_terminate.Set(::deamer::language::type::definition::object::main::NonTerminal("pattern_constructor_terminate", { pattern_constructor_terminate_PATTERN_INSERTION.Pointer(),pattern_constructor_terminate_VARNAME.Pointer(),pattern_constructor_terminate_NUMBER.Pointer(),pattern_constructor_terminate_DECIMAL.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));
pattern_constructor_encapsulation.Set(::deamer::language::type::definition::object::main::NonTerminal("pattern_constructor_encapsulation", { pattern_constructor_encapsulation_LEFT_BRACKET_pattern_constructor_content_RIGHT_BRACKET.Pointer(),pattern_constructor_encapsulation_LEFT_PARANTHESIS_pattern_constructor_content_RIGHT_PARANTHESIS.Pointer(),pattern_constructor_encapsulation_LEFT_SQUARE_BRACKET_pattern_constructor_content_RIGHT_SQUARE_BRACKET.Pointer() } , ::deamer::language::type::definition::object::main::NonTerminalAbstraction::Standard , false));


	// Production-Rule initialization
	deamerreserved_star__stmt___stmt_deamerreserved_star__stmt__.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->stmt.Pointer(),Language->deamerreserved_star__stmt__.Pointer() }));
deamerreserved_star__stmt___EMPTY.Set(::deamer::language::type::definition::object::main::ProductionRule());
program_deamerreserved_star__stmt__.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->deamerreserved_star__stmt__.Pointer() }));
stmt_pattern_constructor.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_constructor.Pointer() }));
stmt_pattern_constructor_array.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_constructor_array.Pointer() }));
stmt_pattern_execution.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_execution.Pointer() }));
pattern_execution_pattern_name_LEFT_BRACKET_pattern_constructor_content_RIGHT_BRACKET.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_name.Pointer(),Language->LEFT_BRACKET.Pointer(),Language->pattern_constructor_content.Pointer(),Language->RIGHT_BRACKET.Pointer() }));
deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array_____pattern_constructor.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_constructor.Pointer() }));
deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array_____pattern_constructor_array.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_constructor_array.Pointer() }));
deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array_____pattern_constructor_deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_constructor.Pointer(),Language->deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____.Pointer() }));
deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array_____pattern_constructor_array_deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_constructor_array.Pointer(),Language->deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____.Pointer() }));
pattern_constructor_array_pattern_type_COLON_pattern_name_LEFT_SQUARE_BRACKET_deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array_____RIGHT_SQUARE_BRACKET.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_type.Pointer(),Language->COLON.Pointer(),Language->pattern_name.Pointer(),Language->LEFT_SQUARE_BRACKET.Pointer(),Language->deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____.Pointer(),Language->RIGHT_SQUARE_BRACKET.Pointer() }));
pattern_constructor_pattern_type_COLON_pattern_name_LEFT_BRACKET_pattern_constructor_content_RIGHT_BRACKET.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_type.Pointer(),Language->COLON.Pointer(),Language->pattern_name.Pointer(),Language->LEFT_BRACKET.Pointer(),Language->pattern_constructor_content.Pointer(),Language->RIGHT_BRACKET.Pointer() }));
pattern_type_VARNAME.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->VARNAME.Pointer() }));
deamerreserved_arrow__VARNAME___VARNAME_deamerreserved_star__GT__VARNAME__.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->VARNAME.Pointer(),Language->deamerreserved_star__GT__VARNAME__.Pointer() }));
deamerreserved_star__GT__VARNAME___GT_VARNAME_deamerreserved_star__GT__VARNAME__.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->GT.Pointer(),Language->VARNAME.Pointer(),Language->deamerreserved_star__GT__VARNAME__.Pointer() }));
deamerreserved_star__GT__VARNAME___EMPTY.Set(::deamer::language::type::definition::object::main::ProductionRule());
pattern_name_deamerreserved_arrow__VARNAME__.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->deamerreserved_arrow__VARNAME__.Pointer() }));
deamerreserved_star__pattern_constructor_content_stmt___pattern_constructor_content_stmt_deamerreserved_star__pattern_constructor_content_stmt__.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->pattern_constructor_content_stmt.Pointer(),Language->deamerreserved_star__pattern_constructor_content_stmt__.Pointer() }));
deamerreserved_star__pattern_constructor_content_stmt___EMPTY.Set(::deamer::language::type::definition::object::main::ProductionRule());
pattern_constructor_content_deamerreserved_star__pattern_constructor_content_stmt__.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->deamerreserved_star__pattern_constructor_content_stmt__.Pointer() }));
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
pattern_constructor_operator_AND.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->AND.Pointer() }));
pattern_constructor_operator_OR.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->OR.Pointer() }));
pattern_constructor_operator_EQEQ.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->EQEQ.Pointer() }));
pattern_constructor_operator_EQEQEQ.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->EQEQEQ.Pointer() }));
pattern_constructor_operator_OROR.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->OROR.Pointer() }));
pattern_constructor_operator_ANDAND.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->ANDAND.Pointer() }));
pattern_constructor_operator_WILDCARD_OP.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->WILDCARD_OP.Pointer() }));
pattern_constructor_structure_DOT.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->DOT.Pointer() }));
pattern_constructor_structure_COMMA.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->COMMA.Pointer() }));
pattern_constructor_structure_COLON.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->COLON.Pointer() }));
pattern_constructor_structure_SEMICOLON.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->SEMICOLON.Pointer() }));
pattern_constructor_structure_SIGN.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->SIGN.Pointer() }));
pattern_constructor_structure_HEKJE.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->HEKJE.Pointer() }));
pattern_constructor_structure_QUESTION.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->QUESTION.Pointer() }));
pattern_constructor_structure_EXCLAM.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->EXCLAM.Pointer() }));
pattern_constructor_terminate_PATTERN_INSERTION.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->PATTERN_INSERTION.Pointer() }));
pattern_constructor_terminate_VARNAME.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->VARNAME.Pointer() }));
pattern_constructor_terminate_NUMBER.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->NUMBER.Pointer() }));
pattern_constructor_terminate_DECIMAL.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->DECIMAL.Pointer() }));
pattern_constructor_encapsulation_LEFT_BRACKET_pattern_constructor_content_RIGHT_BRACKET.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->LEFT_BRACKET.Pointer(),Language->pattern_constructor_content.Pointer(),Language->RIGHT_BRACKET.Pointer() }));
pattern_constructor_encapsulation_LEFT_PARANTHESIS_pattern_constructor_content_RIGHT_PARANTHESIS.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->LEFT_PARANTHESIS.Pointer(),Language->pattern_constructor_content.Pointer(),Language->RIGHT_PARANTHESIS.Pointer() }));
pattern_constructor_encapsulation_LEFT_SQUARE_BRACKET_pattern_constructor_content_RIGHT_SQUARE_BRACKET.Set(::deamer::language::type::definition::object::main::ProductionRule({ Language->LEFT_SQUARE_BRACKET.Pointer(),Language->pattern_constructor_content.Pointer(),Language->RIGHT_SQUARE_BRACKET.Pointer() }));


	// Unknown references
	

	// Add object calls
	// AddObject(...)
	AddObject(program);
AddObject(deamerreserved_star__stmt__);
AddObject(stmt);
AddObject(pattern_execution);
AddObject(pattern_constructor_array);
AddObject(deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____);
AddObject(pattern_constructor);
AddObject(pattern_type);
AddObject(pattern_name);
AddObject(deamerreserved_arrow__VARNAME__);
AddObject(deamerreserved_star__GT__VARNAME__);
AddObject(pattern_constructor_content);
AddObject(deamerreserved_star__pattern_constructor_content_stmt__);
AddObject(pattern_constructor_content_stmt);
AddObject(pattern_constructor_operator);
AddObject(pattern_constructor_structure);
AddObject(pattern_constructor_terminate);
AddObject(pattern_constructor_encapsulation);


	AddObject(deamerreserved_star__stmt___stmt_deamerreserved_star__stmt__);
AddObject(deamerreserved_star__stmt___EMPTY);
AddObject(program_deamerreserved_star__stmt__);
AddObject(stmt_pattern_constructor);
AddObject(stmt_pattern_constructor_array);
AddObject(stmt_pattern_execution);
AddObject(pattern_execution_pattern_name_LEFT_BRACKET_pattern_constructor_content_RIGHT_BRACKET);
AddObject(deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array_____pattern_constructor);
AddObject(deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array_____pattern_constructor_array);
AddObject(deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array_____pattern_constructor_deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____);
AddObject(deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array_____pattern_constructor_array_deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____);
AddObject(pattern_constructor_array_pattern_type_COLON_pattern_name_LEFT_SQUARE_BRACKET_deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array_____RIGHT_SQUARE_BRACKET);
AddObject(pattern_constructor_pattern_type_COLON_pattern_name_LEFT_BRACKET_pattern_constructor_content_RIGHT_BRACKET);
AddObject(pattern_type_VARNAME);
AddObject(deamerreserved_arrow__VARNAME___VARNAME_deamerreserved_star__GT__VARNAME__);
AddObject(deamerreserved_star__GT__VARNAME___GT_VARNAME_deamerreserved_star__GT__VARNAME__);
AddObject(deamerreserved_star__GT__VARNAME___EMPTY);
AddObject(pattern_name_deamerreserved_arrow__VARNAME__);
AddObject(deamerreserved_star__pattern_constructor_content_stmt___pattern_constructor_content_stmt_deamerreserved_star__pattern_constructor_content_stmt__);
AddObject(deamerreserved_star__pattern_constructor_content_stmt___EMPTY);
AddObject(pattern_constructor_content_deamerreserved_star__pattern_constructor_content_stmt__);
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
AddObject(pattern_constructor_operator_AND);
AddObject(pattern_constructor_operator_OR);
AddObject(pattern_constructor_operator_EQEQ);
AddObject(pattern_constructor_operator_EQEQEQ);
AddObject(pattern_constructor_operator_OROR);
AddObject(pattern_constructor_operator_ANDAND);
AddObject(pattern_constructor_operator_WILDCARD_OP);
AddObject(pattern_constructor_structure_DOT);
AddObject(pattern_constructor_structure_COMMA);
AddObject(pattern_constructor_structure_COLON);
AddObject(pattern_constructor_structure_SEMICOLON);
AddObject(pattern_constructor_structure_SIGN);
AddObject(pattern_constructor_structure_HEKJE);
AddObject(pattern_constructor_structure_QUESTION);
AddObject(pattern_constructor_structure_EXCLAM);
AddObject(pattern_constructor_terminate_PATTERN_INSERTION);
AddObject(pattern_constructor_terminate_VARNAME);
AddObject(pattern_constructor_terminate_NUMBER);
AddObject(pattern_constructor_terminate_DECIMAL);
AddObject(pattern_constructor_encapsulation_LEFT_BRACKET_pattern_constructor_content_RIGHT_BRACKET);
AddObject(pattern_constructor_encapsulation_LEFT_PARANTHESIS_pattern_constructor_content_RIGHT_PARANTHESIS);
AddObject(pattern_constructor_encapsulation_LEFT_SQUARE_BRACKET_pattern_constructor_content_RIGHT_SQUARE_BRACKET);


	


	// Place higher level operations here.
	// ReplaceObject(..., ...)
	// DeleteObject(..., ...)
	
}
