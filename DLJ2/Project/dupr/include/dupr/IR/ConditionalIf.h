#ifndef DUPR_IR_CONDITIONALIF_H
#define DUPR_IR_CONDITIONALIF_H

#include "dupr/IR/Expression.h"
#include "dupr/IR/ProgramOrder.h"
#include "dupr/IR/Statement.h"
#include <string>
#include <vector>

namespace dupr::ir
{
	class ConditionalIf : public Statement
	{
	private:
		Expression* expression;
		std::vector<Statement*> block;

	public:
		ConditionalIf(Expression* expression_, const std::vector<Statement*>& block_)
			: Statement(Type::ConditionalIf),
			  expression(expression_),
			  block(block_)
		{
		}
		~ConditionalIf()
		{
			delete expression;
		}

	public:
		ir::Expression* GetExpression() const
		{
			return expression;
		}

		std::vector<Statement*> GetStatements() const
		{
			return block;
		}

	public:
		void Print() override
		{
			std::cout << "\tConditional if:\n";
			std::cout << "\t\tExpression: ";
			expression->Print();
			std::cout << "\n";
			for (auto statement : block)
			{
				statement->Print();
			}
		}
	};
}

#endif // DUPR_IR_CONDITIONALIF_H
