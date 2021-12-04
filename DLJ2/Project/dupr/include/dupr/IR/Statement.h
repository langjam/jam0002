#ifndef DUPR_IR_STATEMENT_H
#define DUPR_IR_STATEMENT_H

#include "dupr/IR/Expression.h"
#include <string>

namespace dupr::ir
{
	class Statement
	{
	public:
		enum class Type
		{
			ReturnStatement,
			VariableDeclaration,
			VariableAssignment,
		};

	private:
		Statement::Type type;

	public:
		Statement(Statement::Type type_) : type(type_)
		{
		}

	public:
		Statement::Type GetType() const
		{
			return type;
		}
	};
}

#endif // DUPR_IR_STATEMENT_H
