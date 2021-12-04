#ifndef DUPR_AST_RELATION_NODEENUMTOTYPE_H
#define DUPR_AST_RELATION_NODEENUMTOTYPE_H

#include "dupr/Ast/Enum/Type.h"

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


namespace dupr { namespace ast { namespace relation { 

	template<::dupr::ast::Type T>
	struct NodeEnumToType
	{
		constexpr static auto value = T;
		using type = void;
	};


	template<>
	struct NodeEnumToType<::dupr::ast::Type::program>
	{
		constexpr static auto value = ::dupr::ast::Type::program;
		using type = ::dupr::ast::node::program;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::deamerreserved_star__stmt__>
	{
		constexpr static auto value = ::dupr::ast::Type::deamerreserved_star__stmt__;
		using type = ::dupr::ast::node::deamerreserved_star__stmt__;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::stmt>
	{
		constexpr static auto value = ::dupr::ast::Type::stmt;
		using type = ::dupr::ast::node::stmt;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::pattern_execution>
	{
		constexpr static auto value = ::dupr::ast::Type::pattern_execution;
		using type = ::dupr::ast::node::pattern_execution;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::pattern_constructor_array>
	{
		constexpr static auto value = ::dupr::ast::Type::pattern_constructor_array;
		using type = ::dupr::ast::node::pattern_constructor_array;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____>
	{
		constexpr static auto value = ::dupr::ast::Type::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____;
		using type = ::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::pattern_constructor>
	{
		constexpr static auto value = ::dupr::ast::Type::pattern_constructor;
		using type = ::dupr::ast::node::pattern_constructor;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::pattern_type>
	{
		constexpr static auto value = ::dupr::ast::Type::pattern_type;
		using type = ::dupr::ast::node::pattern_type;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::pattern_name>
	{
		constexpr static auto value = ::dupr::ast::Type::pattern_name;
		using type = ::dupr::ast::node::pattern_name;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::deamerreserved_arrow__VARNAME__>
	{
		constexpr static auto value = ::dupr::ast::Type::deamerreserved_arrow__VARNAME__;
		using type = ::dupr::ast::node::deamerreserved_arrow__VARNAME__;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::deamerreserved_star__GT__VARNAME__>
	{
		constexpr static auto value = ::dupr::ast::Type::deamerreserved_star__GT__VARNAME__;
		using type = ::dupr::ast::node::deamerreserved_star__GT__VARNAME__;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::pattern_constructor_content>
	{
		constexpr static auto value = ::dupr::ast::Type::pattern_constructor_content;
		using type = ::dupr::ast::node::pattern_constructor_content;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::deamerreserved_plus__pattern_constructor_content_stmt__>
	{
		constexpr static auto value = ::dupr::ast::Type::deamerreserved_plus__pattern_constructor_content_stmt__;
		using type = ::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::pattern_constructor_content_stmt>
	{
		constexpr static auto value = ::dupr::ast::Type::pattern_constructor_content_stmt;
		using type = ::dupr::ast::node::pattern_constructor_content_stmt;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::pattern_constructor_operator>
	{
		constexpr static auto value = ::dupr::ast::Type::pattern_constructor_operator;
		using type = ::dupr::ast::node::pattern_constructor_operator;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::pattern_constructor_structure>
	{
		constexpr static auto value = ::dupr::ast::Type::pattern_constructor_structure;
		using type = ::dupr::ast::node::pattern_constructor_structure;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::pattern_constructor_terminate>
	{
		constexpr static auto value = ::dupr::ast::Type::pattern_constructor_terminate;
		using type = ::dupr::ast::node::pattern_constructor_terminate;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::pattern_constructor_encapsulation>
	{
		constexpr static auto value = ::dupr::ast::Type::pattern_constructor_encapsulation;
		using type = ::dupr::ast::node::pattern_constructor_encapsulation;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::LEFT_BRACKET>
	{
		constexpr static auto value = ::dupr::ast::Type::LEFT_BRACKET;
		using type = ::dupr::ast::node::LEFT_BRACKET;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::RIGHT_BRACKET>
	{
		constexpr static auto value = ::dupr::ast::Type::RIGHT_BRACKET;
		using type = ::dupr::ast::node::RIGHT_BRACKET;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::LEFT_PARANTHESIS>
	{
		constexpr static auto value = ::dupr::ast::Type::LEFT_PARANTHESIS;
		using type = ::dupr::ast::node::LEFT_PARANTHESIS;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::RIGHT_PARANTHESIS>
	{
		constexpr static auto value = ::dupr::ast::Type::RIGHT_PARANTHESIS;
		using type = ::dupr::ast::node::RIGHT_PARANTHESIS;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::LEFT_SQUARE_BRACKET>
	{
		constexpr static auto value = ::dupr::ast::Type::LEFT_SQUARE_BRACKET;
		using type = ::dupr::ast::node::LEFT_SQUARE_BRACKET;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::RIGHT_SQUARE_BRACKET>
	{
		constexpr static auto value = ::dupr::ast::Type::RIGHT_SQUARE_BRACKET;
		using type = ::dupr::ast::node::RIGHT_SQUARE_BRACKET;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::ADD>
	{
		constexpr static auto value = ::dupr::ast::Type::ADD;
		using type = ::dupr::ast::node::ADD;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::MINUS>
	{
		constexpr static auto value = ::dupr::ast::Type::MINUS;
		using type = ::dupr::ast::node::MINUS;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::MULTI>
	{
		constexpr static auto value = ::dupr::ast::Type::MULTI;
		using type = ::dupr::ast::node::MULTI;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::DIVIDE>
	{
		constexpr static auto value = ::dupr::ast::Type::DIVIDE;
		using type = ::dupr::ast::node::DIVIDE;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::LT>
	{
		constexpr static auto value = ::dupr::ast::Type::LT;
		using type = ::dupr::ast::node::LT;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::LE>
	{
		constexpr static auto value = ::dupr::ast::Type::LE;
		using type = ::dupr::ast::node::LE;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::GT>
	{
		constexpr static auto value = ::dupr::ast::Type::GT;
		using type = ::dupr::ast::node::GT;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::GE>
	{
		constexpr static auto value = ::dupr::ast::Type::GE;
		using type = ::dupr::ast::node::GE;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::EQ>
	{
		constexpr static auto value = ::dupr::ast::Type::EQ;
		using type = ::dupr::ast::node::EQ;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::OR>
	{
		constexpr static auto value = ::dupr::ast::Type::OR;
		using type = ::dupr::ast::node::OR;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::AND>
	{
		constexpr static auto value = ::dupr::ast::Type::AND;
		using type = ::dupr::ast::node::AND;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::DOT>
	{
		constexpr static auto value = ::dupr::ast::Type::DOT;
		using type = ::dupr::ast::node::DOT;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::COMMA>
	{
		constexpr static auto value = ::dupr::ast::Type::COMMA;
		using type = ::dupr::ast::node::COMMA;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::COLON>
	{
		constexpr static auto value = ::dupr::ast::Type::COLON;
		using type = ::dupr::ast::node::COLON;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::SEMICOLON>
	{
		constexpr static auto value = ::dupr::ast::Type::SEMICOLON;
		using type = ::dupr::ast::node::SEMICOLON;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::SIGN>
	{
		constexpr static auto value = ::dupr::ast::Type::SIGN;
		using type = ::dupr::ast::node::SIGN;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::HEKJE>
	{
		constexpr static auto value = ::dupr::ast::Type::HEKJE;
		using type = ::dupr::ast::node::HEKJE;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::QUESTION>
	{
		constexpr static auto value = ::dupr::ast::Type::QUESTION;
		using type = ::dupr::ast::node::QUESTION;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::EXCLAM>
	{
		constexpr static auto value = ::dupr::ast::Type::EXCLAM;
		using type = ::dupr::ast::node::EXCLAM;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::PATTERN_INSERTION>
	{
		constexpr static auto value = ::dupr::ast::Type::PATTERN_INSERTION;
		using type = ::dupr::ast::node::PATTERN_INSERTION;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::VARNAME>
	{
		constexpr static auto value = ::dupr::ast::Type::VARNAME;
		using type = ::dupr::ast::node::VARNAME;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::NUMBER>
	{
		constexpr static auto value = ::dupr::ast::Type::NUMBER;
		using type = ::dupr::ast::node::NUMBER;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::DECIMAL>
	{
		constexpr static auto value = ::dupr::ast::Type::DECIMAL;
		using type = ::dupr::ast::node::DECIMAL;
	};

	template<>
	struct NodeEnumToType<::dupr::ast::Type::ESCAPE_CHARS>
	{
		constexpr static auto value = ::dupr::ast::Type::ESCAPE_CHARS;
		using type = ::dupr::ast::node::ESCAPE_CHARS;
	};


template<::dupr::ast::Type T>
constexpr static auto NodeEnumToType_v = ::dupr::ast::relation::NodeEnumToType<T>::value;

template<::dupr::ast::Type T>
using NodeEnumToType_t = typename ::dupr::ast::relation::NodeEnumToType<T>::type;

}}}

#endif // DUPR_AST_RELATION_NODEENUMTOTYPE_H