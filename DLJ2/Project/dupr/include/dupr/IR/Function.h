#ifndef DUPR_IR_FUNCTION_H
#define DUPR_IR_FUNCTION_H

#include "dupr/IR/Argument.h"
#include "dupr/IR/ProgramOrder.h"
#include "dupr/IR/Statement.h"
#include <string>
#include <vector>

namespace dupr::ir
{
	class Function : public ProgramOrder
	{
	private:
		std::string return_type;
		std::string name;
		std::vector<Argument> arguments;
		std::vector<Statement> block;

	public:
		Function(const std::string& return_type_, const std::string& name_,
				 const std::vector<Argument>& arguments_, const std::vector<Statement>& block_)
			: ProgramOrder(OrderType::Function),
			  return_type(return_type_),
			  name(name_),
			  arguments(arguments_),
			  block(block_)
		{
		}

	public:
		void Print() override
		{
			std::cout << "Function:\n"
					  << "\tReturnType: " + return_type + "\n"
					  << "\tName: " + name + "\n"
					  << "\tArguments:\n";
			for (auto argument : arguments)
			{
				std::cout << "\t\tType: " + argument.GetType() + "\n";
				std::cout << "\t\tName: " + argument.GetName() + "\n";
				std::cout << "\n";
			}

			std::cout << "\tStatements: -\n";
		}
	};
}

#endif // DUPR_IR_FUNCTION_H
