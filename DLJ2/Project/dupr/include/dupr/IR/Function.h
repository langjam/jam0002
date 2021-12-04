#ifndef DUPR_IR_FUNCTION_H
#define DUPR_IR_FUNCTION_H

#include "dupr/IR/Argument.h"
#include <string>
#include <vector>

namespace dupr::ir
{
	class Function
	{
	private:
		std::string return_type;
		std::string name;
		std::vector<Argument> arguments;

	public:
		Function(const std::string& return_type_, const std::string& name_,
				 const std::vector<Argument>& arguments_)
			: return_type(return_type_),
			  name(name_),
			  arguments(arguments_)
		{
		}

	public:
	};
}

#endif // DUPR_IR_FUNCTION_H
