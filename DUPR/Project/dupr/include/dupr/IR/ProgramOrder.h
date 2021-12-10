#ifndef DUPR_IR_PROGRAMORDER_H
#define DUPR_IR_PROGRAMORDER_H

namespace dupr::ir
{
	enum class OrderType
	{
		Unknown,
		Function,
		Statement,
		Expression,
		ConditionalIf,
		ConditionalElseIf,
		ConditionalElse,
	};

	class ProgramOrder
	{
	private:
		OrderType type = OrderType::Unknown;

	public:
		ProgramOrder(OrderType type_) : type(type_)
		{
		}
		virtual ~ProgramOrder() = default;

		OrderType GetOrderType() const
		{
			return type;
		}

		virtual void Print()
		{
		}
	};
}

#endif // DUPR_IR_PROGRAMORDER_H
