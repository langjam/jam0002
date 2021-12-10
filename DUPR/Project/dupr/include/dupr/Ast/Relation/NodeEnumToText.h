#ifndef DUPR_AST_RELATION_NODEENUMTOTEXT_H
#define DUPR_AST_RELATION_NODEENUMTOTEXT_H

#include "dupr/Ast/Enum/Type.h"

namespace dupr { namespace ast { namespace relation { 

	static constexpr const char* ConvertEnumToText(::dupr::ast::Type type)
	{
		switch(type)
		{
			
	case ::dupr::ast::Type::program: {
		return "program";
	}
	

	case ::dupr::ast::Type::deamerreserved_star__stmt__: {
		return "deamerreserved_star__stmt__";
	}
	

	case ::dupr::ast::Type::stmt: {
		return "stmt";
	}
	

	case ::dupr::ast::Type::pattern_execution: {
		return "pattern_execution";
	}
	

	case ::dupr::ast::Type::pattern_constructor_array: {
		return "pattern_constructor_array";
	}
	

	case ::dupr::ast::Type::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____: {
		return "deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____";
	}
	

	case ::dupr::ast::Type::pattern_constructor: {
		return "pattern_constructor";
	}
	

	case ::dupr::ast::Type::pattern_type: {
		return "pattern_type";
	}
	

	case ::dupr::ast::Type::pattern_name: {
		return "pattern_name";
	}
	

	case ::dupr::ast::Type::deamerreserved_arrow__VARNAME__: {
		return "deamerreserved_arrow__VARNAME__";
	}
	

	case ::dupr::ast::Type::deamerreserved_star__GT__VARNAME__: {
		return "deamerreserved_star__GT__VARNAME__";
	}
	

	case ::dupr::ast::Type::pattern_constructor_content: {
		return "pattern_constructor_content";
	}
	

	case ::dupr::ast::Type::deamerreserved_star__pattern_constructor_content_stmt__: {
		return "deamerreserved_star__pattern_constructor_content_stmt__";
	}
	

	case ::dupr::ast::Type::pattern_constructor_content_stmt: {
		return "pattern_constructor_content_stmt";
	}
	

	case ::dupr::ast::Type::pattern_constructor_operator: {
		return "pattern_constructor_operator";
	}
	

	case ::dupr::ast::Type::pattern_constructor_structure: {
		return "pattern_constructor_structure";
	}
	

	case ::dupr::ast::Type::pattern_constructor_terminate: {
		return "pattern_constructor_terminate";
	}
	

	case ::dupr::ast::Type::pattern_constructor_encapsulation: {
		return "pattern_constructor_encapsulation";
	}
	

	case ::dupr::ast::Type::LEFT_BRACKET: {
		return "LEFT_BRACKET";
	}
	

	case ::dupr::ast::Type::RIGHT_BRACKET: {
		return "RIGHT_BRACKET";
	}
	

	case ::dupr::ast::Type::LEFT_PARANTHESIS: {
		return "LEFT_PARANTHESIS";
	}
	

	case ::dupr::ast::Type::RIGHT_PARANTHESIS: {
		return "RIGHT_PARANTHESIS";
	}
	

	case ::dupr::ast::Type::LEFT_SQUARE_BRACKET: {
		return "LEFT_SQUARE_BRACKET";
	}
	

	case ::dupr::ast::Type::RIGHT_SQUARE_BRACKET: {
		return "RIGHT_SQUARE_BRACKET";
	}
	

	case ::dupr::ast::Type::ADD: {
		return "ADD";
	}
	

	case ::dupr::ast::Type::MINUS: {
		return "MINUS";
	}
	

	case ::dupr::ast::Type::MULTI: {
		return "MULTI";
	}
	

	case ::dupr::ast::Type::DIVIDE: {
		return "DIVIDE";
	}
	

	case ::dupr::ast::Type::LT: {
		return "LT";
	}
	

	case ::dupr::ast::Type::LE: {
		return "LE";
	}
	

	case ::dupr::ast::Type::GT: {
		return "GT";
	}
	

	case ::dupr::ast::Type::GE: {
		return "GE";
	}
	

	case ::dupr::ast::Type::EQ: {
		return "EQ";
	}
	

	case ::dupr::ast::Type::EQEQ: {
		return "EQEQ";
	}
	

	case ::dupr::ast::Type::EQEQEQ: {
		return "EQEQEQ";
	}
	

	case ::dupr::ast::Type::OR: {
		return "OR";
	}
	

	case ::dupr::ast::Type::AND: {
		return "AND";
	}
	

	case ::dupr::ast::Type::OROR: {
		return "OROR";
	}
	

	case ::dupr::ast::Type::ANDAND: {
		return "ANDAND";
	}
	

	case ::dupr::ast::Type::WILDCARD_OP: {
		return "WILDCARD_OP";
	}
	

	case ::dupr::ast::Type::DOT: {
		return "DOT";
	}
	

	case ::dupr::ast::Type::COMMA: {
		return "COMMA";
	}
	

	case ::dupr::ast::Type::COLON: {
		return "COLON";
	}
	

	case ::dupr::ast::Type::SEMICOLON: {
		return "SEMICOLON";
	}
	

	case ::dupr::ast::Type::SIGN: {
		return "SIGN";
	}
	

	case ::dupr::ast::Type::HEKJE: {
		return "HEKJE";
	}
	

	case ::dupr::ast::Type::QUESTION: {
		return "QUESTION";
	}
	

	case ::dupr::ast::Type::EXCLAM: {
		return "EXCLAM";
	}
	

	case ::dupr::ast::Type::PATTERN_INSERTION: {
		return "PATTERN_INSERTION";
	}
	

	case ::dupr::ast::Type::VARNAME: {
		return "VARNAME";
	}
	

	case ::dupr::ast::Type::NUMBER: {
		return "NUMBER";
	}
	

	case ::dupr::ast::Type::DECIMAL: {
		return "DECIMAL";
	}
	

	case ::dupr::ast::Type::STRING: {
		return "STRING";
	}
	

	case ::dupr::ast::Type::ESCAPE_CHARS: {
		return "ESCAPE_CHARS";
	}
	

		}
		
		return "";
	}

}}}

#endif // DUPR_AST_RELATION_NODEENUMTOTEXT_H