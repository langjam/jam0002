#ifndef DUPR_IR_CONDITIONALELSE_H
#define DUPR_IR_CONDITIONALELSE_H

#include "dupr/IR/ProgramOrder.h"
#include "dupr/IR/Statement.h"
#include <string>
#include <vector>

namespace dupr::ir
{
	class ConditionalElse : public Statement
	{
	private:
		std::vector<Statement*> block;

	public:
		ConditionalElse(const std::vector<Statement*>& block_)
			: Statement(Type::ConditionalElse),
			  block(block_)
		{
		}

	public:
		std::vector<Statement*> GetStatements() const
		{
			return block;
		}

	public:
		void Print() override
		{
			std::cout << "\tConditional else:\n";
			for (auto statement : block)
			{
				statement->Print();
			}
		}
	};
}

#endif // DUPR_IR_CONDITIONALELSE_H
