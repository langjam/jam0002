#ifndef DUPR_IR_VALUE_H
#define DUPR_IR_VALUE_H

#include "dupr/IR/Expression.h"
#include <string>

namespace dupr::ir
{
	class Value : public Expression
	{
	private:
		std::string value;

	public:
		Value(const std::string& value_) : Expression(Expression::Type::Value), value(value_)
		{
		}

	public:
	};
}

#endif // DUPR_IR_VALUE_H
