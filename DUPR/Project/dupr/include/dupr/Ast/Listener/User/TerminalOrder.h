#ifndef DUPR_AST_LISTENER_USER_TERMINALORDER_H
#define DUPR_AST_LISTENER_USER_TERMINALORDER_H

#include "dupr/Ast/Listener/EnterExitListener.h"
#include <vector>

namespace dupr::ast::listener::user
{
	class TerminalOrder : public EnterExitListener
	{
	private:
		std::vector<const ::deamer::external::cpp::ast::Node*> nodes;

	public:
		TerminalOrder() = default;

	public:
		void EnterTerminal(const ::deamer::external::cpp::ast::Node* node) override
		{
			nodes.push_back(node);
		}

		std::vector<const ::deamer::external::cpp::ast::Node*> GetNodeOrder() const
		{
			return nodes;
		}
	};
}

#endif // DUPR_AST_LISTENER_USER_TERMINALORDER_H
