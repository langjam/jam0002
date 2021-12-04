#ifndef DUPR_AST_RELATION_NODETYPETOENUM_H
#define DUPR_AST_RELATION_NODETYPETOENUM_H

#include "dupr/Ast/Enum/Type.h"

namespace dupr { namespace ast { namespace node {

class program;
class deamerreserved_star__stmt__;
class stmt;
class pattern_execution;
class pattern_execution_content;
class deamerreserved_plus__pattern_execution_content_stmt__;
class pattern_execution_content_stmt;
class pattern_constructor;
class pattern_type;
class pattern_name;
class pattern_constructor_content;
class deamerreserved_plus__pattern_constructor_content_stmt__;
class pattern_constructor_content_stmt;
class pattern_constructor_operator;
class pattern_constructor_structure;
class pattern_constructor_terminate;
class pattern_constructor_encapsulation;
class LEFT_BRACKET;
class RIGHT_BRACKET;
class LEFT_PARANTHESIS;
class RIGHT_PARANTHESIS;
class LEFT_SQUARE_BRACKET;
class RIGHT_SQUARE_BRACKET;
class ADD;
class MINUS;
class MULTI;
class DIVIDE;
class LT;
class LE;
class GT;
class GE;
class EQ;
class DOT;
class COMMA;
class COLON;
class SEMICOLON;
class PATTERN_INSERTION;
class VARNAME;
class NUMBER;
class DECIMAL;
class ESCAPE_CHARS;

}}}

namespace dupr { namespace ast { namespace relation { 

	template<typename T>
	struct NodeTypeToEnum
	{
		constexpr static auto value = ::dupr::ast::Type::deamerreserved_unknown;
		using type = void;
	};


	template<>
	struct NodeTypeToEnum<::dupr::ast::node::program>
	{
		constexpr static auto value = ::dupr::ast::Type::program;
		using type = ::dupr::ast::node::program;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::deamerreserved_star__stmt__>
	{
		constexpr static auto value = ::dupr::ast::Type::deamerreserved_star__stmt__;
		using type = ::dupr::ast::node::deamerreserved_star__stmt__;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::stmt>
	{
		constexpr static auto value = ::dupr::ast::Type::stmt;
		using type = ::dupr::ast::node::stmt;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::pattern_execution>
	{
		constexpr static auto value = ::dupr::ast::Type::pattern_execution;
		using type = ::dupr::ast::node::pattern_execution;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::pattern_execution_content>
	{
		constexpr static auto value = ::dupr::ast::Type::pattern_execution_content;
		using type = ::dupr::ast::node::pattern_execution_content;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::deamerreserved_plus__pattern_execution_content_stmt__>
	{
		constexpr static auto value = ::dupr::ast::Type::deamerreserved_plus__pattern_execution_content_stmt__;
		using type = ::dupr::ast::node::deamerreserved_plus__pattern_execution_content_stmt__;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::pattern_execution_content_stmt>
	{
		constexpr static auto value = ::dupr::ast::Type::pattern_execution_content_stmt;
		using type = ::dupr::ast::node::pattern_execution_content_stmt;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::pattern_constructor>
	{
		constexpr static auto value = ::dupr::ast::Type::pattern_constructor;
		using type = ::dupr::ast::node::pattern_constructor;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::pattern_type>
	{
		constexpr static auto value = ::dupr::ast::Type::pattern_type;
		using type = ::dupr::ast::node::pattern_type;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::pattern_name>
	{
		constexpr static auto value = ::dupr::ast::Type::pattern_name;
		using type = ::dupr::ast::node::pattern_name;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::pattern_constructor_content>
	{
		constexpr static auto value = ::dupr::ast::Type::pattern_constructor_content;
		using type = ::dupr::ast::node::pattern_constructor_content;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__>
	{
		constexpr static auto value = ::dupr::ast::Type::deamerreserved_plus__pattern_constructor_content_stmt__;
		using type = ::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::pattern_constructor_content_stmt>
	{
		constexpr static auto value = ::dupr::ast::Type::pattern_constructor_content_stmt;
		using type = ::dupr::ast::node::pattern_constructor_content_stmt;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::pattern_constructor_operator>
	{
		constexpr static auto value = ::dupr::ast::Type::pattern_constructor_operator;
		using type = ::dupr::ast::node::pattern_constructor_operator;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::pattern_constructor_structure>
	{
		constexpr static auto value = ::dupr::ast::Type::pattern_constructor_structure;
		using type = ::dupr::ast::node::pattern_constructor_structure;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::pattern_constructor_terminate>
	{
		constexpr static auto value = ::dupr::ast::Type::pattern_constructor_terminate;
		using type = ::dupr::ast::node::pattern_constructor_terminate;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::pattern_constructor_encapsulation>
	{
		constexpr static auto value = ::dupr::ast::Type::pattern_constructor_encapsulation;
		using type = ::dupr::ast::node::pattern_constructor_encapsulation;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::LEFT_BRACKET>
	{
		constexpr static auto value = ::dupr::ast::Type::LEFT_BRACKET;
		using type = ::dupr::ast::node::LEFT_BRACKET;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::RIGHT_BRACKET>
	{
		constexpr static auto value = ::dupr::ast::Type::RIGHT_BRACKET;
		using type = ::dupr::ast::node::RIGHT_BRACKET;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::LEFT_PARANTHESIS>
	{
		constexpr static auto value = ::dupr::ast::Type::LEFT_PARANTHESIS;
		using type = ::dupr::ast::node::LEFT_PARANTHESIS;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::RIGHT_PARANTHESIS>
	{
		constexpr static auto value = ::dupr::ast::Type::RIGHT_PARANTHESIS;
		using type = ::dupr::ast::node::RIGHT_PARANTHESIS;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::LEFT_SQUARE_BRACKET>
	{
		constexpr static auto value = ::dupr::ast::Type::LEFT_SQUARE_BRACKET;
		using type = ::dupr::ast::node::LEFT_SQUARE_BRACKET;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::RIGHT_SQUARE_BRACKET>
	{
		constexpr static auto value = ::dupr::ast::Type::RIGHT_SQUARE_BRACKET;
		using type = ::dupr::ast::node::RIGHT_SQUARE_BRACKET;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::ADD>
	{
		constexpr static auto value = ::dupr::ast::Type::ADD;
		using type = ::dupr::ast::node::ADD;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::MINUS>
	{
		constexpr static auto value = ::dupr::ast::Type::MINUS;
		using type = ::dupr::ast::node::MINUS;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::MULTI>
	{
		constexpr static auto value = ::dupr::ast::Type::MULTI;
		using type = ::dupr::ast::node::MULTI;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::DIVIDE>
	{
		constexpr static auto value = ::dupr::ast::Type::DIVIDE;
		using type = ::dupr::ast::node::DIVIDE;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::LT>
	{
		constexpr static auto value = ::dupr::ast::Type::LT;
		using type = ::dupr::ast::node::LT;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::LE>
	{
		constexpr static auto value = ::dupr::ast::Type::LE;
		using type = ::dupr::ast::node::LE;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::GT>
	{
		constexpr static auto value = ::dupr::ast::Type::GT;
		using type = ::dupr::ast::node::GT;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::GE>
	{
		constexpr static auto value = ::dupr::ast::Type::GE;
		using type = ::dupr::ast::node::GE;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::EQ>
	{
		constexpr static auto value = ::dupr::ast::Type::EQ;
		using type = ::dupr::ast::node::EQ;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::DOT>
	{
		constexpr static auto value = ::dupr::ast::Type::DOT;
		using type = ::dupr::ast::node::DOT;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::COMMA>
	{
		constexpr static auto value = ::dupr::ast::Type::COMMA;
		using type = ::dupr::ast::node::COMMA;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::COLON>
	{
		constexpr static auto value = ::dupr::ast::Type::COLON;
		using type = ::dupr::ast::node::COLON;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::SEMICOLON>
	{
		constexpr static auto value = ::dupr::ast::Type::SEMICOLON;
		using type = ::dupr::ast::node::SEMICOLON;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::PATTERN_INSERTION>
	{
		constexpr static auto value = ::dupr::ast::Type::PATTERN_INSERTION;
		using type = ::dupr::ast::node::PATTERN_INSERTION;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::VARNAME>
	{
		constexpr static auto value = ::dupr::ast::Type::VARNAME;
		using type = ::dupr::ast::node::VARNAME;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::NUMBER>
	{
		constexpr static auto value = ::dupr::ast::Type::NUMBER;
		using type = ::dupr::ast::node::NUMBER;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::DECIMAL>
	{
		constexpr static auto value = ::dupr::ast::Type::DECIMAL;
		using type = ::dupr::ast::node::DECIMAL;
	};

	template<>
	struct NodeTypeToEnum<::dupr::ast::node::ESCAPE_CHARS>
	{
		constexpr static auto value = ::dupr::ast::Type::ESCAPE_CHARS;
		using type = ::dupr::ast::node::ESCAPE_CHARS;
	};


template<typename T>
constexpr static auto NodeTypeToEnum_v = ::dupr::ast::relation::NodeTypeToEnum<T>::value;

template<typename T>
using NodeTypeToEnum_t = typename ::dupr::ast::relation::NodeTypeToEnum<T>::type;

}}}

#endif // DUPR_AST_RELATION_NODETYPETOENUM_H