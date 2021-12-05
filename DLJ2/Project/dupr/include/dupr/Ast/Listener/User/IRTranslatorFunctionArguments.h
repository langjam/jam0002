#ifndef DUPR_AST_LISTENER_USER_IRTRANSLATORFUNCTIONARGUMENTS_H
#define DUPR_AST_LISTENER_USER_IRTRANSLATORFUNCTIONARGUMENTS_H

#include "dupr/Ast/Listener/EnterExitListener.h"
#include "dupr/Ast/Listener/User/TerminalOrder.h"
#include "dupr/Ast/Reference/Access.h"
#include "dupr/IR/Function.h"
#include "dupr/IR/Table.h"
#include <optional>
#include <string>

namespace dupr::ast::listener::user
{
	class IRTranslator;

	class IRTranslatorFunctionArguments : public EnterExitListener
	{
	private:
		std::vector<ir::Argument> arguments;

	public:
		const dupr::ast::node::pattern_constructor_content* content = nullptr;
		std::vector<const dupr::ast::node::pattern_constructor_content_stmt*> statements;
		bool encapsulated_visited = false;

		// self fixing encapsulated_visited
		void ListenEntry(const dupr::ast::node::pattern_constructor_encapsulation* node) override
		{
			encapsulated_visited = true;
		}
		void ListenExit(const dupr::ast::node::pattern_constructor_encapsulation* node) override
		{
			encapsulated_visited = false;
		}

		void ListenEntry(const dupr::ast::node::pattern_constructor_content* node) override
		{
			if (!encapsulated_visited)
				content = node;
		}
		void ListenEntry(const dupr::ast::node::pattern_constructor_content_stmt* node) override
		{
			if (!encapsulated_visited)
				statements.push_back(node);
		}

	public:
		IRTranslatorFunctionArguments() = default;

		bool ParseExtension(const std::vector<const deamer::external::cpp::ast::Node*>& nodes,
							IRTranslator* irTranslator, const std::string& patternDirection);
		bool ParseArgument(const std::vector<const deamer::external::cpp::ast::Node*>& nodes,
						   IRTranslator* irTranslator, const std::string& patternDirection);
		bool IsLeftPositional(const std::string& cs);
		bool IsRightPositional(const std::string& cs);
		bool GetArguments(IRTranslator* irTranslator, std::string patternDirection,
						  std::vector<const ::deamer::external::cpp::ast::Node*> nodes);

		bool IsExtensionStateMachineEmpty(IRTranslator* irTranslator, const std::string& cs);
	};
}

#endif // DUPR_AST_LISTENER_USER_IRTRANSLATORFUNCTIONARGUMENTS_H
