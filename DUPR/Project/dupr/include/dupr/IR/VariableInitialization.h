#ifndef DUPR_IR_VARIABLEINITIALIZATION_H
#define DUPR_IR_VARIABLEINITIALIZATION_H

#include "dupr/IR/Expression.h"
#include "dupr/IR/Statement.h"
#include <iostream>
#include <string>

namespace dupr::ir
{
	class VariableInitialization : public Statement
	{
	private:
		std::string name;
		std::string type;
		Expression* value;

	public:
		VariableInitialization(const std::string& name_, const std::string& type_,
							   Expression* value_)
			: Statement(Statement::Type::VariableInitialization),
			  name(name_),
			  type(type_),
			  value(value_)
		{
		}

	public:
		void Print() override
		{
			std::cout << "\t\tVariable Initialization:\n";
			std::cout << "\t\t\tName: " + name + "\n";
			std::cout << "\t\t\tValue: ";
			value->Print();
			std::cout << "\n";
		}

		std::string GetVariableName() const
		{
			return name;
		}

		std::string GetVariableType() const
		{
			return type;
		}

		ir::Expression* GetExpression() const
		{
			return value;
		}
	};
}

#endif // DUPR_IR_VARIABLEINITIALIZATION_H
