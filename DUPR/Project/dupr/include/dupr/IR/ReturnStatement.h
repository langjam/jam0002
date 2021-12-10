#ifndef DUPR_IR_RETURNSTATEMENT_H
#define DUPR_IR_RETURNSTATEMENT_H

#include "dupr/IR/Expression.h"
#include "dupr/IR/Statement.h"
#include <string>

namespace dupr::ir
{
	class ReturnStatement : public Statement
	{
	private:
		Expression* expression;

	public:
		ReturnStatement(Expression* expression_)
			: Statement(Statement::Type::ReturnStatement),
			  expression(expression_)
		{
		}

	public:
		void Print() override
		{
			std::cout << "\t\tReturn statement: ";
			expression->Print();
			std::cout << "\n";
		}

		ir::Expression* GetExpression()
		{
			return expression;
		}
	};
}

#endif // DUPR_IR_RETURNSTATEMENT_H
