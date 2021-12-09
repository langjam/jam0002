#ifndef DUPR_AST_RELATION_NODETEXTTOENUM_H
#define DUPR_AST_RELATION_NODETEXTTOENUM_H

#include "dupr/Ast/Enum/Type.h"

namespace dupr { namespace ast { namespace relation { 

	static constexpr ::dupr::ast::Type ConvertEnumToText(const std::string& text)
	{
		static const std::map<std::string, ::dupr::ast::Type> mapTextWithType = {
			{"", ::dupr::ast::Type::deamerreserved_unknown },
			
		{ "program", ::dupr::ast::Type::program },
	

		{ "deamerreserved_star__stmt__", ::dupr::ast::Type::deamerreserved_star__stmt__ },
	

		{ "stmt", ::dupr::ast::Type::stmt },
	

		{ "pattern_execution", ::dupr::ast::Type::pattern_execution },
	

		{ "pattern_constructor_array", ::dupr::ast::Type::pattern_constructor_array },
	

		{ "deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____", ::dupr::ast::Type::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____ },
	

		{ "pattern_constructor", ::dupr::ast::Type::pattern_constructor },
	

		{ "pattern_type", ::dupr::ast::Type::pattern_type },
	

		{ "pattern_name", ::dupr::ast::Type::pattern_name },
	

		{ "deamerreserved_arrow__VARNAME__", ::dupr::ast::Type::deamerreserved_arrow__VARNAME__ },
	

		{ "deamerreserved_star__GT__VARNAME__", ::dupr::ast::Type::deamerreserved_star__GT__VARNAME__ },
	

		{ "pattern_constructor_content", ::dupr::ast::Type::pattern_constructor_content },
	

		{ "deamerreserved_star__pattern_constructor_content_stmt__", ::dupr::ast::Type::deamerreserved_star__pattern_constructor_content_stmt__ },
	

		{ "pattern_constructor_content_stmt", ::dupr::ast::Type::pattern_constructor_content_stmt },
	

		{ "pattern_constructor_operator", ::dupr::ast::Type::pattern_constructor_operator },
	

		{ "pattern_constructor_structure", ::dupr::ast::Type::pattern_constructor_structure },
	

		{ "pattern_constructor_terminate", ::dupr::ast::Type::pattern_constructor_terminate },
	

		{ "pattern_constructor_encapsulation", ::dupr::ast::Type::pattern_constructor_encapsulation },
	

		{ "LEFT_BRACKET", ::dupr::ast::Type::LEFT_BRACKET },
	

		{ "RIGHT_BRACKET", ::dupr::ast::Type::RIGHT_BRACKET },
	

		{ "LEFT_PARANTHESIS", ::dupr::ast::Type::LEFT_PARANTHESIS },
	

		{ "RIGHT_PARANTHESIS", ::dupr::ast::Type::RIGHT_PARANTHESIS },
	

		{ "LEFT_SQUARE_BRACKET", ::dupr::ast::Type::LEFT_SQUARE_BRACKET },
	

		{ "RIGHT_SQUARE_BRACKET", ::dupr::ast::Type::RIGHT_SQUARE_BRACKET },
	

		{ "ADD", ::dupr::ast::Type::ADD },
	

		{ "MINUS", ::dupr::ast::Type::MINUS },
	

		{ "MULTI", ::dupr::ast::Type::MULTI },
	

		{ "DIVIDE", ::dupr::ast::Type::DIVIDE },
	

		{ "LT", ::dupr::ast::Type::LT },
	

		{ "LE", ::dupr::ast::Type::LE },
	

		{ "GT", ::dupr::ast::Type::GT },
	

		{ "GE", ::dupr::ast::Type::GE },
	

		{ "EQ", ::dupr::ast::Type::EQ },
	

		{ "EQEQ", ::dupr::ast::Type::EQEQ },
	

		{ "EQEQEQ", ::dupr::ast::Type::EQEQEQ },
	

		{ "OR", ::dupr::ast::Type::OR },
	

		{ "AND", ::dupr::ast::Type::AND },
	

		{ "OROR", ::dupr::ast::Type::OROR },
	

		{ "ANDAND", ::dupr::ast::Type::ANDAND },
	

		{ "WILDCARD_OP", ::dupr::ast::Type::WILDCARD_OP },
	

		{ "DOT", ::dupr::ast::Type::DOT },
	

		{ "COMMA", ::dupr::ast::Type::COMMA },
	

		{ "COLON", ::dupr::ast::Type::COLON },
	

		{ "SEMICOLON", ::dupr::ast::Type::SEMICOLON },
	

		{ "SIGN", ::dupr::ast::Type::SIGN },
	

		{ "HEKJE", ::dupr::ast::Type::HEKJE },
	

		{ "QUESTION", ::dupr::ast::Type::QUESTION },
	

		{ "EXCLAM", ::dupr::ast::Type::EXCLAM },
	

		{ "PATTERN_INSERTION", ::dupr::ast::Type::PATTERN_INSERTION },
	

		{ "VARNAME", ::dupr::ast::Type::VARNAME },
	

		{ "NUMBER", ::dupr::ast::Type::NUMBER },
	

		{ "DECIMAL", ::dupr::ast::Type::DECIMAL },
	

		{ "STRING", ::dupr::ast::Type::STRING },
	

		{ "ESCAPE_CHARS", ::dupr::ast::Type::ESCAPE_CHARS },
	

		};

		return mapTextWithType[text];
	}

}}}

#endif // DUPR_AST_RELATION_NODETEXTTOENUM_H