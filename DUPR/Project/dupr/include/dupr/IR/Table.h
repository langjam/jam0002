#ifndef DUPR_IR_TABLE_H
#define DUPR_IR_TABLE_H

#include "dupr/IR/ProgramOrder.h"

namespace dupr::ir
{
	class Table
	{
	private:
		std::vector<ProgramOrder*> programOrder;

	public:
		Table() = default;
		~Table()
		{
			for (auto* order : programOrder)
			{
				delete order;
			}
		}

	public:
		void Add(ProgramOrder* programOrder_)
		{
			programOrder.push_back(programOrder_);
		}

		std::vector<ProgramOrder*> Get(OrderType type)
		{
			std::vector<ProgramOrder*> foundMatches;
			for (auto order : programOrder)
			{
				if (order->GetOrderType() == type)
				{
					foundMatches.push_back(order);
				}
			}

			return foundMatches;
		}

		void Print()
		{
			for (auto order : programOrder)
			{
				order->Print();
			}
		}
	};
}

#endif // DUPR_IR_TABLE_H
