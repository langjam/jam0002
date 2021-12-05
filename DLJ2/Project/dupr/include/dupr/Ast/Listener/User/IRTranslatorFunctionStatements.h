#ifndef DUPR_AST_LISTENER_USER_IRTRANSLATORFUNCTIONSTATEMENTS_H
#define DUPR_AST_LISTENER_USER_IRTRANSLATORFUNCTIONSTATEMENTS_H

#include "dupr/Ast/Listener/EnterExitListener.h"
#include "dupr/Ast/Listener/User/TerminalOrder.h"
#include "dupr/Ast/Reference/Access.h"
#include "dupr/IR/Function.h"
#include "dupr/IR/Table.h"
#include <optional>
#include <string>
#include <vector>

namespace dupr::ast::listener::user
{
	class IRTranslator;
	class PatternStateMachine;

	class IRTranslatorFunctionStatements : public EnterExitListener
	{
	private:
		IRTranslator* irTranslator;
		std::string patternDirection;

	public:
		IRTranslatorFunctionStatements(IRTranslator* irTranslator_, std::string patternDirection_);
		~IRTranslatorFunctionStatements() = default;

	public:
		const dupr::ast::node::pattern_constructor_content* content = nullptr;
		std::vector<const dupr::ast::node::pattern_constructor_content_stmt*> statements;
		std::size_t encapsulated_visited = 0;

		// self fixing encapsulated_visited
		void ListenEntry(const dupr::ast::node::pattern_constructor_encapsulation* node) override
		{
			encapsulated_visited += 1;
		}
		void ListenExit(const dupr::ast::node::pattern_constructor_encapsulation* node) override
		{
			encapsulated_visited -= 1;
		}

		void ListenEntry(const dupr::ast::node::pattern_constructor_content* node) override
		{
			if (encapsulated_visited == 0)
				content = node;
		}
		void ListenEntry(const dupr::ast::node::pattern_constructor_content_stmt* node) override
		{
			if (encapsulated_visited == 0)
				statements.push_back(node);
		}

		bool GetStatements(std::vector<const ::deamer::external::cpp::ast::Node*> nodes);
		std::vector<PatternStateMachine*> GetAllStatementStatemachines(const std::string& pattern);
	};
}

#endif // DUPR_AST_LISTENER_USER_IRTRANSLATORFUNCTIONSTATEMENTS_H
