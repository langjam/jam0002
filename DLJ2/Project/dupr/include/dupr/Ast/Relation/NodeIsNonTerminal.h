#ifndef DUPR_AST_RELATION_NODEISNONTERMINAL_H
#define DUPR_AST_RELATION_NODEISNONTERMINAL_H

#include "dupr/Ast/Enum/Type.h"

namespace dupr { namespace ast { namespace relation { 

	constexpr static bool NodeIsNonTerminal(::dupr::ast::Type t)
	{
		
		if (t == ::dupr::ast::Type::program)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::deamerreserved_star__stmt__)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::stmt)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::pattern_execution)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::pattern_constructor_array)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::pattern_constructor)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::pattern_type)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::pattern_name)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::deamerreserved_arrow__VARNAME__)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::deamerreserved_star__GT__VARNAME__)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::pattern_constructor_content)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::deamerreserved_plus__pattern_constructor_content_stmt__)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::pattern_constructor_content_stmt)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::pattern_constructor_operator)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::pattern_constructor_structure)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::pattern_constructor_terminate)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::pattern_constructor_encapsulation)
		{
			return true;
		}


		return false;
	}


}}}

#endif // DUPR_AST_RELATION_NODEISNONTERMINAL_H