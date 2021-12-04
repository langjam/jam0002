#ifndef DUPR_IR_STATEMENT_H
#define DUPR_IR_STATEMENT_H

#include "dupr/IR/ProgramOrder.h"
#include <string>

namespace dupr::ir
{
	class Statement : public ProgramOrder
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
		Statement(Statement::Type type_) : ProgramOrder(OrderType::Statement), type(type_)
		{
		}
		virtual ~Statement() = default;

	public:
		Statement::Type GetType() const
		{
			return type;
		}
	};
}

#endif // DUPR_IR_STATEMENT_H
