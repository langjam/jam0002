#ifndef DUPR_IR_EXPRESSION_H
#define DUPR_IR_EXPRESSION_H

#include "dupr/IR/ProgramOrder.h"
#include <string>

namespace dupr::ir
{
	class Expression : public ProgramOrder
	{
	public:
		enum class Type
		{
			Expression,
			Value,
		};

	private:
		Expression* lhs;
		Expression* rhs;
		std::string operation;
		Expression::Type type;

	public:
		Expression(Expression::Type type_) : Expression(nullptr, nullptr, "", type_)
		{
		}
		virtual ~Expression() = default;

		Expression(Expression* lhs_, Expression* rhs_, const std::string& operation_,
				   Expression::Type type_ = Expression::Type::Expression)
			: ProgramOrder(OrderType::Expression),
			  lhs(lhs_),
			  rhs(rhs_),
			  operation(operation_),
			  type(type_)
		{
		}
	};
}

#endif // DUPR_IR_EXPRESSION_H
