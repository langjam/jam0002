#ifndef DUPR_AST_LISTENER_USER_IRTRANSLATOR_H
#define DUPR_AST_LISTENER_USER_IRTRANSLATOR_H

#include "dupr/Ast/Listener/EnterExitListener.h"
#include "dupr/Ast/Listener/User/IRTranslatorFunctionArguments.h"
#include "dupr/Ast/Listener/User/TerminalOrder.h"
#include "dupr/Ast/Reference/Access.h"
#include "dupr/IR/Function.h"
#include "dupr/IR/Table.h"
#include <optional>
#include <string>

namespace dupr::ast::listener::user
{
	std::string GetPatternInsertion(std::string stateName);

	std::string GetPatternDirection(std::string stateName);

	std::vector<std::string> GetPatternDirections(std::string stateName);

	template<typename T>
	reference::Access<T> GetAccessor(const T* t)
	{
		return {t};
	}

	struct PatternStateMachine
	{
		std::string type;
		std::string name;
		std::vector<std::function<void(PatternStateMachine*)>> callbacks;

		PatternStateMachine(const std::string& type_, const std::string& name_,
							std::function<void(PatternStateMachine*)> callback_)
			: type(type_),
			  name(name_),
			  callbacks({callback_})
		{
		}

		void AddCallback(std::function<void(PatternStateMachine*)> callback_)
		{
			callbacks.push_back(callback_);
		}

		bool IsEmpty() const
		{
			return states.empty();
		}

		enum class StateType
		{
			any_word,
			predefined_formatter,
			expression_tree,
		};

		class State
		{
		public:
			State(StateType type_, std::string predefined_format_,
				  std::function<void(State*)> callback_)
				: callback(callback_),
				  type(type_),
				  predefined_format(predefined_format_)
			{
			}

			StateType GetType() const
			{
				return type;
			}

			std::optional<std::string> GetPredefinedFormat() const
			{
				return predefined_format;
			}

			std::vector<const ::deamer::external::cpp::ast::Node*> GetNode() const
			{
				return nodeValues;
			}

			void AddNode(const ::deamer::external::cpp::ast::Node* nodeValue_)
			{
				nodeValues.push_back(nodeValue_);
			}

			void ResetNodes()
			{
				nodeValues.clear();
			}

			bool IsStartOfEncapsulation() const
			{
				return predefined_format == "(" || predefined_format == "{" ||
					   predefined_format == "[";
			}

			bool IsEndOfEncapsulation() const
			{
				return predefined_format == ")" || predefined_format == "}" ||
					   predefined_format == "]";
			}

		private:
			std::function<void(State*)> callback;
			std::vector<const ::deamer::external::cpp::ast::Node*> nodeValues;
			StateType type;
			std::string predefined_format;
		};
		std::vector<State*> states;
		~PatternStateMachine()
		{
			for (auto* state : states)
			{
				delete state;
			}
		}

		std::string GetType() const
		{
			return type;
		}

		std::string GetName() const
		{
			return name;
		}

		std::vector<State*> GetStates() const
		{
			return states;
		}

		bool Check(const std::vector<const ::deamer::external::cpp::ast::Node*>& currentNodes,
				   bool execute = false, bool print = true)
		{
			std::size_t index = 0;
			auto currentState = GetCurrentState(index);
			auto nextState = GetNextState(index);
			if (print)
			{
				std::cout << "\tPatternType: " + type + "\n\tPatternName: " + name + "\n";
			}
			if (print)
			{
				std::cout << "\tParsing (ignoring escape chars): ";
				for (auto currentNode : currentNodes)
				{
					std::cout << currentNode->GetText();
				}
				std::cout << "\n";
			}

			for (auto statement : currentNodes)
			{
				// It means the input given is not accepted with our current state machine.
				if (index >= states.size())
				{
					if (print)
					{
						std::cout << "\tGiven input cannot be accepted by pattern, all states are "
									 "already "
									 "visited.\n";
					}
					return false;
				}

				if (statement->GetType() ==
					static_cast<std::size_t>(dupr::ast::Type::pattern_constructor_content_stmt))
				{
					statement = statement->GetIndex(0);
				}
				auto text = statement->GetText();

				if (print)
				{
					std::cout << "\t\tCurrent Value: " << text << "\n";
				}
				if (statement->GetType() ==
					static_cast<std::size_t>(dupr::ast::Type::pattern_constructor_encapsulation))
				{
					const auto* terminatingNT =
						static_cast<const node::pattern_constructor_encapsulation*>(statement);
					const bool valid = Check(terminatingNT, index, execute);

					if (!valid)
					{
						if (print)
						{
							std::cout << "\tPattern failed at '" + terminatingNT->GetText() +
											 "': encapsulation\n";
						}
						return false;
					}
				}
				if (statement->GetType() ==
					static_cast<std::size_t>(dupr::ast::Type::pattern_constructor_operator))
				{
					const auto* terminatingNT =
						static_cast<const node::pattern_constructor_operator*>(statement);
					const bool valid = Check(terminatingNT, index, execute);

					if (!valid)
					{
						if (print)
						{
							std::cout << "\tPattern failed at '" + terminatingNT->GetText() +
											 "': operator, expected: " +
											 GetCurrentState(index)->GetPredefinedFormat().value() +
											 "\n";
						}
						return false;
					}
				}
				if (statement->GetType() ==
					static_cast<std::size_t>(dupr::ast::Type::pattern_constructor_terminate))
				{
					const auto* terminatingNT =
						static_cast<const node::pattern_constructor_terminate*>(statement);
					const bool valid = Check(terminatingNT, index, execute);

					if (!valid)
					{
						if (print)
						{
							std::cout << "\tPattern failed at '" + terminatingNT->GetText() +
											 "': termination, expected: " +
											 GetCurrentState(index)->GetPredefinedFormat().value() +
											 "\n";
						}
						return false;
					}
				}
				if (statement->GetType() ==
					static_cast<std::size_t>(dupr::ast::Type::pattern_constructor_structure))
				{
					const auto* terminatingNT =
						static_cast<const node::pattern_constructor_structure*>(statement);
					const bool valid = Check(terminatingNT, index, execute);

					if (!valid)
					{
						if (print)
						{
							std::cout << "\tPattern failed at '" + terminatingNT->GetText() +
											 "': structure, expected: " +
											 GetCurrentState(index)->GetPredefinedFormat().value() +
											 "\n";
						}
						return false;
					}
				}
			}

			if (index == states.size())
			{
				return true;
			}
			else
			{
				std::cout << "\tGiven input cannot be accepted by pattern, not all states are "
							 "visited.\n";
				return false;
			}
		}

		void Execute(const std::vector<const ::deamer::external::cpp::ast::Node*>& currentNodes)
		{
			ResetNodesOfStates();
			Check(currentNodes, true, false);
			for (auto callback : callbacks)
			{
				callback(this);
			}
		}

		bool Check(const dupr::ast::node::pattern_constructor_content* currentNode,
				   bool execute = false, bool print = true)
		{
			std::size_t index = 0;
			auto currentState = GetCurrentState(index);
			auto nextState = GetNextState(index);
			if (print)
			{
				std::cout << "\tPatternType: " + type + "\n\tPatternName: " + name + "\n";
			}
			if (print)
			{
				std::cout << "\tParsing (ignoring escape chars): " << currentNode->GetText()
						  << "\n";
			}
			auto statements = GetAccessor(currentNode).pattern_constructor_content_stmt();
			for (std::size_t statement_index = 0; statement_index < statements.GetContent().size();
				 statement_index++)
			{
				// It means the input given is not accepted with our current state machine.
				if (index >= states.size())
				{
					if (print)
					{
						std::cout << "\tGiven input cannot be accepted by pattern, all states are "
									 "already "
									 "visited.\n";
					}
					return false;
				}

				auto statement = GetAccessor(statements.GetContent()[statement_index]);
				auto text = statement.GetContent()[0]->GetText();

				if (print)
				{
					std::cout << "\t\tCurrent Value: " << text << "\n";
				}
				if (!statement.pattern_constructor_encapsulation().GetContent().empty())
				{
					const auto* terminatingNT =
						statement.pattern_constructor_encapsulation().GetContent()[0];
					const bool valid = Check(terminatingNT, index, execute);

					if (!valid)
					{
						if (print)
						{
							std::cout << "\tPattern failed at '" + terminatingNT->GetText() +
											 "': encapsulation\n";
						}
						return false;
					}
				}
				if (!statement.pattern_constructor_operator().GetContent().empty())
				{
					const auto* terminatingNT =
						statement.pattern_constructor_operator().GetContent()[0];
					const bool valid = Check(terminatingNT, index, execute);

					if (!valid)
					{
						if (print)
						{
							std::cout << "\tPattern failed at '" + terminatingNT->GetText() +
											 "': operator, expected: " +
											 GetCurrentState(index)->GetPredefinedFormat().value() +
											 "\n";
						}
						return false;
					}
				}
				if (!statement.pattern_constructor_terminate().GetContent().empty())
				{
					const auto* terminatingNT =
						statement.pattern_constructor_terminate().GetContent()[0];
					const bool valid = Check(terminatingNT, index, execute);

					if (!valid)
					{
						if (print)
						{
							std::cout << "\tPattern failed at '" + terminatingNT->GetText() +
											 "': termination, expected: " +
											 GetCurrentState(index)->GetPredefinedFormat().value() +
											 "\n";
						}
						return false;
					}
				}
				if (!statement.pattern_constructor_structure().GetContent().empty())
				{
					const auto* terminatingNT =
						statement.pattern_constructor_structure().GetContent()[0];
					const bool valid = Check(terminatingNT, index, execute);

					if (!valid)
					{
						if (print)
						{
							std::cout << "\tPattern failed at '" + terminatingNT->GetText() +
											 "': structure, expected: " +
											 GetCurrentState(index)->GetPredefinedFormat().value() +
											 "\n";
						}
						return false;
					}
				}
			}

			if (index == states.size())
			{
				return true;
			}
			else
			{
				std::cout << "\tGiven input cannot be accepted by pattern, not all states are "
							 "visited.\n";
				return false;
			}
		}

		bool Check(const dupr::ast::node::pattern_constructor_encapsulation* currentNode,
				   std::size_t& index, bool execute) const;

		bool Check(const dupr::ast::node::pattern_constructor_operator* currentNode,
				   std::size_t& index, bool execute) const
		{
			bool valid = true;
			const auto currentState = GetCurrentState(index);
			const auto nextState = GetNextState(index);
			switch (currentState->GetType())
			{
			case StateType::any_word: {
				valid = false;
				break;
			}
			case StateType::expression_tree: {
				if (nextState.has_value() &&
					(nextState.value()->GetType() == StateType::any_word ||
					 nextState.value()->GetType() == StateType::expression_tree))
				{
					std::cout << "Unparsable situation!\n";
					valid = false;
				}
				else if (nextState.has_value() &&
						 GetAccessor(currentNode).GetContent()[0]->GetText() ==
							 nextState.value()->GetPredefinedFormat())
				{
					if (execute)
					{
						GetNextState(index).value()->AddNode(currentNode);
					}
					// do logic for the predefined part
					index += 2; // skip states include the predefined part
					valid = true;
				}
				else
				{
					if (execute)
					{
						GetCurrentState(index)->AddNode(currentNode);
					}
					valid = true;
				}

				break;
			}
			case StateType::predefined_formatter: {
				if (GetAccessor(currentNode).GetContent()[0]->GetText() !=
					currentState->GetPredefinedFormat().value())
				{
					valid = false;
				}
				else
				{
					index += 1;
					valid = true;
				}
				break;
			}
			}

			return valid;
		}

		bool Check(const dupr::ast::node::pattern_constructor_structure* currentNode,
				   std::size_t& index, bool execute) const
		{
			bool valid = true;
			const auto currentState = GetCurrentState(index);
			const auto nextState = GetNextState(index);
			switch (currentState->GetType())
			{
			case StateType::any_word: {
				valid = false;
				break;
			}
			case StateType::expression_tree: {
				if (nextState.has_value() && GetAccessor(currentNode).GetContent()[0]->GetText() ==
												 nextState.value()->GetPredefinedFormat())
				{
					if (execute)
					{
						GetNextState(index).value()->AddNode(currentNode);
					}
					// do logic for the predefined part
					index += 2; // skip states include the predefined part
					valid = true;
				}
				else
				{
					valid = false;
				}
				break;
			}
			case StateType::predefined_formatter: {
				if (GetAccessor(currentNode).GetContent()[0]->GetText() !=
					currentState->GetPredefinedFormat().value())
				{
					valid = false;
				}
				else
				{
					if (execute)
					{
						GetCurrentState(index)->AddNode(currentNode);
					}
					index += 1;
					valid = true;
				}
				break;
			}
			}

			return valid;
		}

		bool Check(const dupr::ast::node::pattern_constructor_terminate* currentNode,
				   std::size_t& index, bool execute) const
		{
			bool valid = true;
			const auto currentState = GetCurrentState(index);
			const auto nextState = GetNextState(index);
			switch (currentState->GetType())
			{
			case StateType::any_word: {
				if (execute)
				{
					GetCurrentState(index)->AddNode(currentNode);
				}
				index += 1;
				valid = true;
				break;
			}
			case StateType::expression_tree: {
				if (nextState.has_value() &&
					(nextState.value()->GetType() == StateType::any_word ||
					 nextState.value()->GetType() == StateType::expression_tree))
				{
					std::cout << "Unparsable situation!\n";
					valid = false;
				}
				else if (nextState.has_value() &&
						 GetAccessor(currentNode).GetContent()[0]->GetText() ==
							 nextState.value()->GetPredefinedFormat())
				{
					if (execute)
					{
						GetNextState(index).value()->AddNode(currentNode);
					}
					// do logic for the predefined part
					index += 2; // skip states include the predefined part
					valid = true;
				}
				else
				{
					if (execute)
					{
						GetCurrentState(index)->AddNode(currentNode);
					}
					valid = true;
				}

				break;
			}
			case StateType::predefined_formatter: {
				if (GetAccessor(currentNode).GetContent()[0]->GetText() !=
					currentState->GetPredefinedFormat().value())
				{
					valid = false;
				}
				else
				{
					if (execute)
					{
						GetCurrentState(index)->AddNode(currentNode);
					}
					index += 1;
					valid = true;
				}
				break;
			}
			}

			return valid;
		}

		State* GetCurrentState(std::size_t index) const
		{
			if (index < states.size())
			{
				return states[index];
			}

			throw std::logic_error("There are no states left.");
		}

		std::optional<State*> GetNextState(std::size_t index) const
		{
			if ((index + 1) < states.size())
			{
				return states[index + 1];
			}

			return std::nullopt;
		}

		void ResetNodesOfStates()
		{
			for (auto* state : states)
			{
				state->ResetNodes();
			}
		}

		void Execute(const node::pattern_constructor_content* patternConstructorContent)
		{
			ResetNodesOfStates();
			Check(patternConstructorContent, true, false);
			for (auto callback : callbacks)
			{
				callback(this);
			}
		}

		std::vector<State*> GetStatesWithName(std::string name)
		{
			std::vector<State*> foundMatches;
			for (auto* state : states)
			{
				if (state->GetPredefinedFormat().value() == name)
				{
					foundMatches.push_back(state);
				}
			}

			return foundMatches;
		}

		std::vector<State*> GetPatternInsertionWithName(std::string name)
		{
			std::vector<State*> foundMatches;
			for (auto* state : states)
			{
				if (GetPatternInsertion(state->GetPredefinedFormat().value()) == name)
				{
					foundMatches.push_back(state);
				}
			}

			return foundMatches;
		}
	};

	class IRTranslator : public EnterExitListener
	{
	public:
		std::unique_ptr<ir::Table> irTable;
		std::map<std::string, std::vector<PatternStateMachine*>> mapNameWithPattern;

	public:
		IRTranslator()
		{
			irTable = std::make_unique<ir::Table>();
		}

		~IRTranslator()
		{
			for (auto [name, stateMachines] : mapNameWithPattern)
			{
				for (auto* stateMachine : stateMachines)
				{
					delete stateMachine;
				}
			}
		}

	public:
		std::vector<std::string> directionDepth;
		std::map<std::string, std::vector<std::string>> mapDirectionWithPatterns;

		void ListenEntry(const dupr::ast::node::pattern_constructor_array* node) override
		{
			std::string currentDirection;
			for (auto dirDepth : directionDepth)
			{
				currentDirection += dirDepth;
			}

			const auto extension =
				GetAccessor(node).pattern_name().GetContent()[0]->GetText() + ">";
			directionDepth.push_back(extension);
			std::string newDirection;
			for (auto dirDepth : directionDepth)
			{
				newDirection += dirDepth;
			}

			auto iter = mapDirectionWithPatterns.find(currentDirection);
			if (iter == mapDirectionWithPatterns.end())
			{
				mapDirectionWithPatterns.insert({currentDirection, {newDirection}});
			}
			else
			{
				iter->second.push_back(newDirection);
			}
		}

		void ListenExit(const dupr::ast::node::pattern_constructor_array* node) override
		{
			directionDepth.pop_back();
		}

		std::vector<ir::Argument> functionArguments;
		void
		ListenEntry(const dupr::ast::node::pattern_constructor* node_pattern_constructor) override
		{
			auto accessor = GetAccessor(node_pattern_constructor);
			TerminalOrder terminalOrder;
			terminalOrder.Dispatch(accessor.pattern_constructor_content().GetContent()[0]);

			std::string direction;
			for (auto dirDepth : directionDepth)
			{
				direction += dirDepth;
			}
			auto nodeOrder = terminalOrder.GetNodeOrder();

			auto stateMachine = new PatternStateMachine(
				accessor.pattern_type().GetContent()[0]->GetText(),
				direction + accessor.pattern_name().GetContent()[0]->GetText(),
				[this](PatternStateMachine* stateMachine) {
					if (stateMachine->GetType() == "FunctionPattern")
					{
						ConstructFunction(stateMachine);
					}
					else if (stateMachine->GetType() == "ConditionalIfPattern")
					{
						ConstructConditionalIf(stateMachine);
					}
					else if (stateMachine->GetType() == "ConditionalElseIfPattern")
					{
						ConstructConditionalElseIf(stateMachine);
					}
					else if (stateMachine->GetType() == "ConditionalElsePattern")
					{
						ConstructConditionalElse(stateMachine);
					}
					else if (stateMachine->GetType() == "VariableDeclarationPattern")
					{
						ConstructVariableDeclaration(stateMachine);
					}
					else if (stateMachine->GetType() == "VariableAssignmentPattern")
					{
						ConstructVariableAssignment(stateMachine);
					}
					else if (stateMachine->GetType() == "ReturnPattern")
					{
						ConstructReturnStatement(stateMachine);
					}
					else if (stateMachine->GetType() == "ArgumentExtensionPattern")
					{
						ConstructFunctionArgumentExtension(stateMachine);
					}
					else if (stateMachine->GetType() == "ArgumentContentPattern")
					{
						ConstructFunctionArgument(stateMachine);
					}
					else
					{
						ConstructUnknown(stateMachine);
					}
				});
			for (const auto* node : nodeOrder)
			{
				if (static_cast<dupr::ast::Type>(node->GetType()) ==
					dupr::ast::Type::PATTERN_INSERTION)
				{
					if (GetPatternInsertion(node->GetText()) == "expression")
					{
						stateMachine->states.push_back(new PatternStateMachine::State(
							PatternStateMachine::StateType::expression_tree, node->GetText(),
							[this](PatternStateMachine::State*) {

							}));
					}
					else if (GetPatternInsertion(node->GetText()) == "arguments")
					{
						stateMachine->states.push_back(new PatternStateMachine::State(
							PatternStateMachine::StateType::expression_tree, node->GetText(),
							[this](PatternStateMachine::State*) {

							}));
					}
					else if (GetPatternInsertion(node->GetText()) == "statements")
					{
						stateMachine->states.push_back(new PatternStateMachine::State(
							PatternStateMachine::StateType::expression_tree, node->GetText(),
							[this](PatternStateMachine::State*) {

							}));
					}
					else
					{
						stateMachine->states.push_back(new PatternStateMachine::State(
							PatternStateMachine::StateType::any_word, node->GetText(),
							[this](PatternStateMachine::State*) {

							}));
					}
				}
				else
				{
					stateMachine->states.push_back(new PatternStateMachine::State(
						PatternStateMachine::StateType::predefined_formatter, node->GetText(),
						[this](PatternStateMachine::State*) {

						}));
				}
			}
			auto iter = mapNameWithPattern.find(stateMachine->name);
			if (iter == mapNameWithPattern.end())
			{
				mapNameWithPattern.insert({stateMachine->name, {stateMachine}});
			}
			else
			{
				iter->second.push_back(stateMachine);
			}
		}

		void ListenEntry(const dupr::ast::node::pattern_execution* node) override
		{
			const auto patternName = GetAccessor(node).pattern_name().GetContent()[0]->GetText();
			const auto iter = mapNameWithPattern.find(patternName);
			if (iter == mapNameWithPattern.end())
			{
				std::cout << "No such pattern: " + patternName + "!, Ignoring the given pattern.\n";
				return;
			}

			bool valid = false;
			for (auto stateMachine : iter->second)
			{
				valid = stateMachine->Check(
					GetAccessor(node).pattern_constructor_content().GetContent()[0]);
				if (valid)
				{
					stateMachine->Execute(
						GetAccessor(node).pattern_constructor_content().GetContent()[0]);
					break;
				}
			}

			if (valid)
			{
				std::cout << "Pattern is valid!\n";
			}
			else
			{
				std::cout << "Pattern is invalid!\n";
			}
		}

		bool CheckAreEqual(const std::vector<PatternStateMachine::State*>& vector)
		{
			if (vector.empty())
			{
				return true;
			}

			const auto iFirst = vector[0]->GetNode();
			for (auto* i : vector)
			{
				if (i->GetNode().size() != iFirst.size())
				{
					return false;
				}

				for (auto index = 0; index < iFirst.size(); index++)
				{
					if (i->GetNode()[index]->GetText() != iFirst[index]->GetText())
					{
						return false;
					}
				}
			}

			return true;
		}

		void ConstructFunction(PatternStateMachine* stateMachine)
		{
			std::cout << "Constructing a function:\n";
			auto return_type = stateMachine->GetPatternInsertionWithName("return_type");
			auto name = stateMachine->GetPatternInsertionWithName("name");
			auto arguments = stateMachine->GetPatternInsertionWithName("arguments");
			auto statements = stateMachine->GetPatternInsertionWithName("statements");

			bool error = false;
			if (return_type.empty())
			{
				std::cout << "\tPattern did not match any [[return_type]]\n";
				error = true;
			}
			else
			{
				if (!CheckAreEqual(return_type))
				{
					std::cout << "\tUser used different values for [[return_type]]\n";
					error = true;
				}
			}
			if (name.empty())
			{
				std::cout << "\tPattern did not match any [[name]]\n";
				error = true;
			}
			else
			{
				if (!CheckAreEqual(name))
				{
					std::cout << "\tUser used different values for [[name]]\n";
					error = true;
				}
			}
			if (arguments.empty())
			{
				std::cout << "\tPattern did not match any [[arguments]]\n";
				error = true;
			}
			else
			{
				if (!CheckAreEqual(arguments))
				{
					std::cout << "\tUser used different values for [[arguments]]\n";
					error = true;
				}
			}
			if (statements.empty())
			{
				std::cout << "\tPattern did not match any [[statements]]\n";
				error = true;
			}
			else
			{
				if (!CheckAreEqual(statements))
				{
					std::cout << "\tUser used different values for [[statements]]\n";
					error = true;
				}
			}

			if (error)
			{
				std::cout << "\tNot continuing construction of function, reason: Content did not "
							 "properly match expectations.\n";
				return;
			}

			if (!ParseArguments(arguments[0]))
			{
				std::cout << "\tParsing arguments failed, aborting function construction\n";
				return;
			}

			std::vector<ir::Argument> arguments_parsed = functionArguments;
			std::vector<ir::Statement> statements_parsed;

			irTable->Add(new dupr::ir::Function(return_type[0]->GetNode()[0]->GetText(),
												name[0]->GetNode()[0]->GetText(), arguments_parsed,
												statements_parsed));
		}

		void ConstructVariableAssignment(PatternStateMachine* stateMachine)
		{
			std::cout << "Constructing a variable assignment:\n";
		}

		void ConstructVariableDeclaration(PatternStateMachine* stateMachine)
		{
			std::cout << "Constructing a variable declaration:\n";
		}

		void ConstructReturnStatement(PatternStateMachine* stateMachine)
		{
			std::cout << "Constructing a Return statement:\n";
		}

		void ConstructConditionalIf(PatternStateMachine* stateMachine)
		{
			std::cout << "Constructing a conditional if:\n";
		}

		void ConstructConditionalElseIf(PatternStateMachine* stateMachine)
		{
			std::cout << "Constructing a conditional else if:\n";
		}

		void ConstructConditionalElse(PatternStateMachine* stateMachine)
		{
			std::cout << "Constructing a conditional else if:\n";
		}

		void ConstructUnknown(PatternStateMachine* stateMachine)
		{
			std::cout << "Unknown pattern recognized:" + stateMachine->GetType() + "\n";
		}

		void ConstructFunctionArgument(PatternStateMachine* stateMachine)
		{
			std::cout << "Constructing a function argument:\n";
			auto type = stateMachine->GetPatternInsertionWithName("type");
			auto name = stateMachine->GetPatternInsertionWithName("name");

			bool error = false;
			if (type.empty())
			{
				std::cout << "\tPattern did not match any [[type]]\n";
				error = true;
			}
			else
			{
				if (!CheckAreEqual(type))
				{
					std::cout << "\tUser used different values for [[type]]\n";
					error = true;
				}
			}
			if (name.empty())
			{
				std::cout << "\tPattern did not match any [[name]]\n";
				error = true;
			}
			else
			{
				if (!CheckAreEqual(name))
				{
					std::cout << "\tUser used different values for [[name]]\n";
					error = true;
				}
			}

			if (error)
			{
				std::cout << "\t\tNot continuing construction of function argument, reason: "
							 "Content did not "
							 "properly match expectations.\n";
				return;
			}

			functionArguments.emplace_back(type[0]->GetNode()[0]->GetText(),
										   name[0]->GetNode()[0]->GetText());
		}

		void ConstructFunctionArgumentExtension(PatternStateMachine* stateMachine)
		{
			std::cout << "Constructing a function argument extension:\n";
		}

		bool ParseArguments(PatternStateMachine::State* arguments)
		{
			functionArguments.clear();

			std::cout << "\tRetrieving Arguments using pattern:\n";
			std::cout << "\t\tPattern direction: "
					  << GetPatternDirection(arguments->GetPredefinedFormat().value()) << "\n";
			std::cout << "\t\tPattern insertion: "
					  << GetPatternInsertion(arguments->GetPredefinedFormat().value()) << "\n";

			auto functionArguments = IRTranslatorFunctionArguments();
			bool success = functionArguments.GetArguments(
				this, GetPatternDirection(arguments->GetPredefinedFormat().value()),
				arguments->GetNode());

			return success;
		}

		// DO NOT CALL TWICE
		[[nodiscard]] std::unique_ptr<ir::Table> GetTable()
		{
			return std::move(irTable);
		}
	};
}

#endif // DUPR_AST_LISTENER_USER_IRTRANSLATOR_H
