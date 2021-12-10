#ifndef DUPR_IR_VARIABLEASSIGNMENT_H
#define DUPR_IR_VARIABLEASSIGNMENT_H

#include "dupr/IR/Expression.h"
#include "dupr/IR/Statement.h"
#include <string>

namespace dupr::ir
{
	class VariableAssignment : public Statement
	{
	private:
		std::string name;
		Expression* value;

	public:
		VariableAssignment(const std::string& name_, Expression* value_)
			: Statement(Statement::Type::VariableAssignment),
			  name(name_),
			  value(value_)
		{
		}

	public:
		void Print() override
		{
			std::cout << "\t\tVariable Assignment:\n";
			std::cout << "\t\t\tName: " + name + "\n";
			std::cout << "\t\t\tValue: ";
			value->Print();
			std::cout << "\n";
		}

		std::string GetVariableName() const
		{
			return name;
		}

		ir::Expression* GetExpression() const
		{
			return value;
		}
	};
}

#endif // DUPR_IR_VARIABLEASSIGNMENT_H
