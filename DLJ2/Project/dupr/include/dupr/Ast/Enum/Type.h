#ifndef DUPR_AST_ENUM_TYPE_H
#define DUPR_AST_ENUM_TYPE_H

#include <cstddef>

namespace dupr { namespace ast { 

	enum class Type : std::size_t
	{
		// Reserved
		deamerreserved_unknown,

		// Terminal
		LEFT_BRACKET,
		RIGHT_BRACKET,
		LEFT_PARANTHESIS,
		RIGHT_PARANTHESIS,
		LEFT_SQUARE_BRACKET,
		RIGHT_SQUARE_BRACKET,
		ADD,
		MINUS,
		MULTI,
		DIVIDE,
		LT,
		LE,
		GT,
		GE,
		EQ,
		EQEQ,
		EQEQEQ,
		OR,
		AND,
		OROR,
		ANDAND,
		WILDCARD_OP,
		DOT,
		COMMA,
		COLON,
		SEMICOLON,
		SIGN,
		HEKJE,
		QUESTION,
		EXCLAM,
		PATTERN_INSERTION,
		VARNAME,
		NUMBER,
		DECIMAL,
		ESCAPE_CHARS,


		// NonTerminal
		program,
		deamerreserved_star__stmt__,
		stmt,
		pattern_execution,
		pattern_constructor_array,
		deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____,
		pattern_constructor,
		pattern_type,
		pattern_name,
		deamerreserved_arrow__VARNAME__,
		deamerreserved_star__GT__VARNAME__,
		pattern_constructor_content,
		deamerreserved_star__pattern_constructor_content_stmt__,
		pattern_constructor_content_stmt,
		pattern_constructor_operator,
		pattern_constructor_structure,
		pattern_constructor_terminate,
		pattern_constructor_encapsulation,

	};

	static inline bool operator==(std::size_t lhs, ::dupr::ast::Type rhs)
	{
		return lhs == static_cast<std::size_t>(rhs);
	}

	static inline bool operator==(::dupr::ast::Type lhs, std::size_t rhs)
	{
		return static_cast<std::size_t>(lhs) == rhs;
	}

	static inline bool operator==(int lhs, ::dupr::ast::Type rhs)
	{
		return lhs == static_cast<std::size_t>(rhs);
	}

	static inline bool operator==(::dupr::ast::Type lhs, int rhs)
	{
		return static_cast<std::size_t>(lhs) == rhs;
	}
}}

#endif // DUPR_AST_ENUM_TYPE_H