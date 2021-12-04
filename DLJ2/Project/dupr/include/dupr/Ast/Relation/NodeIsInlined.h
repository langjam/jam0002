#ifndef DUPR_AST_RELATION_NODEISINLINED_H
#define DUPR_AST_RELATION_NODEISINLINED_H

#include "dupr/Ast/Enum/Type.h"

namespace dupr { namespace ast { namespace relation { 

	constexpr static bool NodeIsInlined(::dupr::ast::Type t)
	{
		
		if (t == ::dupr::ast::Type::program)
		{
			return false;
		}

		if (t == ::dupr::ast::Type::deamerreserved_star__stmt__)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::stmt)
		{
			return false;
		}

		if (t == ::dupr::ast::Type::pattern_execution)
		{
			return false;
		}

		if (t == ::dupr::ast::Type::pattern_execution_content)
		{
			return false;
		}

		if (t == ::dupr::ast::Type::deamerreserved_plus__pattern_execution_content_stmt__)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::pattern_execution_content_stmt)
		{
			return false;
		}

		if (t == ::dupr::ast::Type::pattern_constructor)
		{
			return false;
		}

		if (t == ::dupr::ast::Type::pattern_type)
		{
			return false;
		}

		if (t == ::dupr::ast::Type::pattern_name)
		{
			return false;
		}

		if (t == ::dupr::ast::Type::pattern_constructor_content)
		{
			return false;
		}

		if (t == ::dupr::ast::Type::deamerreserved_plus__pattern_constructor_content_stmt__)
		{
			return true;
		}

		if (t == ::dupr::ast::Type::pattern_constructor_content_stmt)
		{
			return false;
		}

		if (t == ::dupr::ast::Type::pattern_constructor_operator)
		{
			return false;
		}

		if (t == ::dupr::ast::Type::pattern_constructor_structure)
		{
			return false;
		}

		if (t == ::dupr::ast::Type::pattern_constructor_terminate)
		{
			return false;
		}

		if (t == ::dupr::ast::Type::pattern_constructor_encapsulation)
		{
			return false;
		}


		return false;
	}


}}}

#endif // DUPR_AST_RELATION_NODEISINLINED_H