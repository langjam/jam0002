#ifndef DUPR_IR_ARGUMENT_H
#define DUPR_IR_ARGUMENT_H

#include <string>

namespace dupr::ir
{
	class Argument
	{
	private:
		std::string type;
		std::string name;

	public:
		Argument(const std::string& type_, const std::string& name_) : type(type_), name(name_)
		{
		}

	public:
	};
}

#endif // DUPR_IR_ARGUMENT_H
