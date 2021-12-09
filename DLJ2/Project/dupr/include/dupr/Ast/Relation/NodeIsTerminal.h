#ifndef DUPR_AST_RELATION_NODEISTERMINAL_H
#define DUPR_AST_RELATION_NODEISTERMINAL_H

#include "dupr/Ast/Enum/Type.h"

namespace dupr { namespace ast { namespace relation { 

	constexpr static bool NodeIsTerminal(::dupr::ast::Type t)
	{
		
		if (t == ::dupr::ast::Type::LEFT_BRACKET)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::RIGHT_BRACKET)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::LEFT_PARANTHESIS)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::RIGHT_PARANTHESIS)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::LEFT_SQUARE_BRACKET)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::RIGHT_SQUARE_BRACKET)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::ADD)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::MINUS)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::MULTI)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::DIVIDE)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::LT)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::LE)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::GT)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::GE)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::EQ)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::EQEQ)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::EQEQEQ)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::OR)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::AND)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::OROR)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::ANDAND)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::WILDCARD_OP)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::DOT)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::COMMA)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::COLON)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::SEMICOLON)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::SIGN)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::HEKJE)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::QUESTION)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::EXCLAM)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::PATTERN_INSERTION)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::VARNAME)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::NUMBER)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::DECIMAL)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::ESCAPE_CHARS)
		{
			return true;
		}


		return false;
	}


}}}

#endif // DUPR_AST_RELATION_NODEISTERMINAL_H