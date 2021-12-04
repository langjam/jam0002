#ifndef dupr_AST_ENUM_TYPE_H
#define dupr_AST_ENUM_TYPE_H

namespace dupr { namespace ast {

	enum class Type
	{
		// Reserved
		deamerreserved_unknown,

		// Terminals
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
		DOT,
		COMMA,
		COLON,
		SEMICOLON,
		PATTERN_INSERTION,
		VARNAME,
		NUMBER,
		DECIMAL,
		ESCAPE_CHARS,

		// Non-Terminals
		program,
		deamerreserved_star__stmt__,
		stmt,
		pattern_execution,
		pattern_execution_content,
		deamerreserved_plus__pattern_execution_content_stmt__,
		pattern_execution_content_stmt,
		pattern_constructor,
		pattern_type,
		pattern_name,
		pattern_constructor_content,
		deamerreserved_plus__pattern_constructor_content_stmt__,
		pattern_constructor_content_stmt,
		pattern_constructor_operator,
		pattern_constructor_structure,
		pattern_constructor_terminate,
		pattern_constructor_encapsulation,
	};

}}

#endif // dupr_AST_ENUM_TYPE_H
