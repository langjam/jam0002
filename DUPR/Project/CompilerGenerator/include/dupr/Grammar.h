#ifndef DUPR_GRAMMAR_H
#define DUPR_GRAMMAR_H

#include "Deamer/Language/Generator/Definition/Property/User/Main/Grammar.h"
#include "Deamer/Language/Type/Definition/Object/Special/Uninitialized.h"

namespace dupr
{
	class Language;

	/*!	\class Grammar
	 *
	 *	\brief This contains the grammar LPD of the language dupr
	 *
	 *	\note This is auto-generated via the DLDL definition.
	 */
	class Grammar : public ::deamer::language::generator::definition::property::user::Grammar<
								::dupr::Language>
	{
	public:
		// Non-Terminal declarations
		::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::NonTerminal> program;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::NonTerminal> deamerreserved_star__stmt__;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::NonTerminal> stmt;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::NonTerminal> pattern_execution;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::NonTerminal> pattern_constructor_array;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::NonTerminal> deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::NonTerminal> pattern_constructor;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::NonTerminal> pattern_type;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::NonTerminal> pattern_name;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::NonTerminal> deamerreserved_arrow__VARNAME__;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::NonTerminal> deamerreserved_star__GT__VARNAME__;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::NonTerminal> pattern_constructor_content;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::NonTerminal> deamerreserved_star__pattern_constructor_content_stmt__;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::NonTerminal> pattern_constructor_content_stmt;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::NonTerminal> pattern_constructor_operator;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::NonTerminal> pattern_constructor_structure;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::NonTerminal> pattern_constructor_terminate;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::NonTerminal> pattern_constructor_encapsulation;

	
		// Production-Rule declarations
		::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> deamerreserved_star__stmt___stmt_deamerreserved_star__stmt__;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> deamerreserved_star__stmt___EMPTY;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> program_deamerreserved_star__stmt__;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> stmt_pattern_constructor;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> stmt_pattern_constructor_array;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> stmt_pattern_execution;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_execution_pattern_name_LEFT_BRACKET_pattern_constructor_content_RIGHT_BRACKET;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array_____pattern_constructor;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array_____pattern_constructor_array;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array_____pattern_constructor_deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array_____pattern_constructor_array_deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_array_pattern_type_COLON_pattern_name_LEFT_SQUARE_BRACKET_deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array_____RIGHT_SQUARE_BRACKET;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_pattern_type_COLON_pattern_name_LEFT_BRACKET_pattern_constructor_content_RIGHT_BRACKET;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_type_VARNAME;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> deamerreserved_arrow__VARNAME___VARNAME_deamerreserved_star__GT__VARNAME__;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> deamerreserved_star__GT__VARNAME___GT_VARNAME_deamerreserved_star__GT__VARNAME__;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> deamerreserved_star__GT__VARNAME___EMPTY;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_name_deamerreserved_arrow__VARNAME__;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> deamerreserved_star__pattern_constructor_content_stmt___pattern_constructor_content_stmt_deamerreserved_star__pattern_constructor_content_stmt__;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> deamerreserved_star__pattern_constructor_content_stmt___EMPTY;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_content_deamerreserved_star__pattern_constructor_content_stmt__;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_content_stmt_pattern_constructor_operator;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_content_stmt_pattern_constructor_terminate;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_content_stmt_pattern_constructor_encapsulation;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_content_stmt_pattern_constructor_structure;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_operator_ADD;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_operator_MINUS;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_operator_MULTI;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_operator_DIVIDE;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_operator_LT;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_operator_LE;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_operator_GT;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_operator_GE;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_operator_EQ;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_operator_AND;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_operator_OR;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_operator_EQEQ;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_operator_EQEQEQ;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_operator_OROR;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_operator_ANDAND;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_operator_WILDCARD_OP;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_structure_DOT;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_structure_COMMA;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_structure_COLON;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_structure_SEMICOLON;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_structure_SIGN;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_structure_HEKJE;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_structure_QUESTION;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_structure_EXCLAM;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_terminate_PATTERN_INSERTION;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_terminate_VARNAME;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_terminate_NUMBER;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_terminate_DECIMAL;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_terminate_STRING;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_encapsulation_LEFT_BRACKET_pattern_constructor_content_RIGHT_BRACKET;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_encapsulation_LEFT_PARANTHESIS_pattern_constructor_content_RIGHT_PARANTHESIS;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::ProductionRule> pattern_constructor_encapsulation_LEFT_SQUARE_BRACKET_pattern_constructor_content_RIGHT_SQUARE_BRACKET;

	
	private:
		// Unknown reference declarations
		
	
	public:
		Grammar(dupr::Language* language);

		void GenerateObjects() override;
	};
}

#endif // DUPR_GRAMMAR_H
