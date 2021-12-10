#ifndef DUPR_AST_LISTENER_USER_IRTRANSLATOR_H
#define DUPR_AST_LISTENER_USER_IRTRANSLATOR_H

#include "dupr/Ast/Listener/EnterExitListener.h"
#include "dupr/Ast/Listener/User/IRTranslatorFunctionArguments.h"
#include "dupr/Ast/Listener/User/IRTranslatorFunctionStatements.h"
#include "dupr/Ast/Listener/User/TerminalOrder.h"
#include "dupr/Ast/Reference/Access.h"
#include "dupr/IR/ConditionalElse.h"
#include "dupr/IR/ConditionalElseIf.h"
#include "dupr/IR/ConditionalIf.h"
#include "dupr/IR/Function.h"
#include "dupr/IR/ReturnStatement.h"
#include "dupr/IR/Table.h"
#include "dupr/IR/Value.h"
#include "dupr/IR/VariableAssignment.h"
#include "dupr/IR/VariableDeclaration.h"
#include "dupr/IR/VariableInitialization.h"
#include <memory>
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

		void AddCallback(std::function<void(PatternStateMachine*)> callback_);

		bool IsEmpty() const;

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

			bool IsStateTheSame(const State* const rhs) const
			{
				return this->GetType() == rhs->GetType() &&
					   this->predefined_format == rhs->predefined_format;
			}

		private:
			std::function<void(State*)> callback;
			std::vector<const ::deamer::external::cpp::ast::Node*> nodeValues;
			StateType type;
			std::string predefined_format;
		};
		std::vector<State*> states;
		~PatternStateMachine();

		std::string GetType() const;

		std::string GetName() const;

		std::vector<State*> GetStates() const;

		bool Check(const std::vector<const ::deamer::external::cpp::ast::Node*>& currentNodes,
				   bool execute = false, bool print = true);

		void Execute(const std::vector<const ::deamer::external::cpp::ast::Node*>& currentNodes);

		bool Check(const dupr::ast::node::pattern_constructor_content* currentNode,
				   bool execute = false, bool print = true);

		bool Check(const dupr::ast::node::pattern_constructor_encapsulation* currentNode,
				   std::size_t& index, bool execute);

		bool Check(const dupr::ast::node::pattern_constructor_operator* currentNode,
				   std::size_t& index, bool execute);

		bool Check(const dupr::ast::node::pattern_constructor_structure* currentNode,
				   std::size_t& index, bool execute);

		bool Check(const dupr::ast::node::pattern_constructor_terminate* currentNode,
				   std::size_t& index, bool execute);

		State* GetCurrentState(std::size_t index) const;

		std::optional<State*> GetNextState(std::size_t index) const;

		void ResetNodesOfStates();

		void Execute(const node::pattern_constructor_content* patternConstructorContent);

		std::vector<State*> GetStatesWithName(std::string name);

		std::vector<State*> GetPatternInsertionWithName(std::string name);
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
				reference::Access(node).pattern_name().GetContent()[0]->GetText() + ">";
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
		std::vector<std::vector<ir::Statement*>> functionStatements;
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
			auto iterDir = mapDirectionWithPatterns.find(direction);
			if (iterDir == mapDirectionWithPatterns.end())
			{
				mapDirectionWithPatterns.insert(
					{direction, {direction + accessor.pattern_name().GetContent()[0]->GetText()}});
			}
			else
			{
				iterDir->second.push_back(direction +
										  accessor.pattern_name().GetContent()[0]->GetText());
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
					else if (stateMachine->GetType() == "ConditionalIfPattern" ||
							 stateMachine->GetType() == "IfPattern")
					{
						ConstructConditionalIf(stateMachine);
					}
					else if (stateMachine->GetType() == "ConditionalElseIfPattern" ||
							 stateMachine->GetType() == "ElseIfPattern")
					{
						ConstructConditionalElseIf(stateMachine);
					}
					else if (stateMachine->GetType() == "ConditionalElsePattern" ||
							 stateMachine->GetType() == "ElsePattern")
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
					else if (stateMachine->GetType() == "VariableInitializationPattern")
					{
						ConstructVariableIntialization(stateMachine);
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
			if (statements.empty())
			{
				std::cout << "\tPattern did not match any [[statements]]\n";
				error = true;
			}

			if (error)
			{
				std::cout << "\tNot continuing construction of function, reason: Content did not "
							 "properly match expectations.\n";
				return;
			}

			std::vector<ir::Argument> arguments_parsed;

			for (auto argument : arguments)
			{
				if (!ParseArguments(argument))
				{
					std::cout << "\tParsing arguments failed, aborting function construction\n";
					return;
				}
				for (auto functionArgument : functionArguments)
				{
					arguments_parsed.push_back(functionArgument);
				}
			}

			std::vector<ir::Statement*> statements_parsed;

			for (auto statement : statements)
			{
				if (!ParseStatements(statement))
				{
					std::cout << "\tParsing statements failed, aborting function construction\n";
					return;
				}
				for (auto functionStatement : functionStatements[0])
				{
					statements_parsed.push_back(functionStatement);
				}
				functionStatements.pop_back();
			}

			irTable->Add(new dupr::ir::Function(return_type[0]->GetNode()[0]->GetText(),
												name[0]->GetNode()[0]->GetText(), arguments_parsed,
												statements_parsed));
		}

		void ConstructVariableAssignment(PatternStateMachine* stateMachine)
		{
			std::cout << "Constructing a variable assignment:\n";
			auto nameState = stateMachine->GetPatternInsertionWithName("name");
			if (nameState.size() != 1)
			{
				return;
			}
			std::string name = nameState[0]->GetNode()[0]->GetText();

			auto expressionState = stateMachine->GetPatternInsertionWithName("expression");
			std::string expressionValue;
			if (expressionState.empty() || expressionState.size() > 1)
			{
				throw std::logic_error("Multiple expressions in pattern\n");
			}
			for (auto node : expressionState[0]->GetNode())
			{
				expressionValue += node->GetText() + " ";
			}
			ir::Expression* expression = new ir::Value(expressionValue);
			auto newStatement = new ir::VariableAssignment(name, expression);
			functionStatements[functionStatements.size() - 1].push_back(newStatement);
		}

		void ConstructVariableDeclaration(PatternStateMachine* stateMachine)
		{
			std::cout << "Constructing a variable declaration:\n";
			auto nameState = stateMachine->GetPatternInsertionWithName("name");
			if (nameState.size() != 1)
			{
				return;
			}
			std::string name = nameState[0]->GetNode()[0]->GetText();
			auto typeState = stateMachine->GetPatternInsertionWithName("type");
			if (typeState.size() != 1)
			{
				return;
			}
			std::string type = typeState[0]->GetNode()[0]->GetText();

			auto newStatement = new ir::VariableDeclaration(type, name);
			functionStatements[functionStatements.size() - 1].push_back(newStatement);
		}

		// For simplicity semantic sugar for variable declaration and assignment.
		void ConstructVariableIntialization(PatternStateMachine* stateMachine)
		{
			std::cout << "Constructing a variable initialization:\n";
			std::cout << "Constructing a variable declaration:\n";
			auto nameState = stateMachine->GetPatternInsertionWithName("name");
			if (nameState.size() != 1)
			{
				return;
			}
			std::string name = nameState[0]->GetNode()[0]->GetText();
			auto typeState = stateMachine->GetPatternInsertionWithName("type");
			if (typeState.size() != 1)
			{
				return;
			}
			std::string type = typeState[0]->GetNode()[0]->GetText();

			auto expressionState = stateMachine->GetPatternInsertionWithName("expression");
			std::string expressionValue;
			if (expressionState.empty() || expressionState.size() > 1)
			{
				throw std::logic_error("Multiple expressions in pattern\n");
			}
			for (auto node : expressionState[0]->GetNode())
			{
				expressionValue += node->GetText() + " ";
			}

			ir::Expression* expression = new ir::Value(expressionValue);
			auto newStatement = new ir::VariableInitialization(name, type, expression);
			functionStatements[functionStatements.size() - 1].push_back(newStatement);
		}

		void ConstructReturnStatement(PatternStateMachine* stateMachine)
		{
			std::cout << "Constructing a Return statement:\n";
			auto expressionState = stateMachine->GetPatternInsertionWithName("expression");
			std::string expressionValue;
			if (expressionState.empty() || expressionState.size() > 1)
			{
				throw std::logic_error("Multiple expressions in pattern\n");
			}
			for (auto node : expressionState[0]->GetNode())
			{
				expressionValue += node->GetText() + " ";
			}

			ir::Expression* expression = new ir::Value(expressionValue);
			auto newStatement = new ir::ReturnStatement(expression);
			functionStatements[functionStatements.size() - 1].push_back(newStatement);
		}

		void ConstructConditionalIf(PatternStateMachine* stateMachine)
		{
			std::cout << "Constructing a conditional if:\n";
			auto statementStates = stateMachine->GetPatternInsertionWithName("statements");
			if (statementStates.empty())
			{
				std::cout << "\tThere are no statement states!\n";
			}
			for (auto statementState : statementStates)
			{
				bool succes = ParseStatements(statementState);
				auto statements = functionStatements[functionStatements.size() - 1];
				functionStatements.pop_back();

				auto expressionState = stateMachine->GetPatternInsertionWithName("expression");
				std::string expressionValue;
				if (expressionState.empty() || expressionState.size() > 1)
				{
					throw std::logic_error("Multiple expressions in pattern\n");
				}
				for (auto node : expressionState[0]->GetNode())
				{
					expressionValue += node->GetText() + " ";
				}
				ir::Expression* expression = new ir::Value(expressionValue);
				auto newConditionalStatement = new ir::ConditionalIf(expression, statements);
				functionStatements[functionStatements.size() - 1].push_back(
					newConditionalStatement);
			}
		}

		void ConstructConditionalElseIf(PatternStateMachine* stateMachine)
		{
			std::cout << "Constructing a conditional else if:\n";
			auto statementStates = stateMachine->GetPatternInsertionWithName("statements");
			if (statementStates.empty())
			{
				std::cout << "\tThere are no statement states!\n";
			}
			for (auto statementState : statementStates)
			{
				bool succes = ParseStatements(statementState);
				auto statements = functionStatements[functionStatements.size() - 1];
				functionStatements.pop_back();

				// Expressions are just the result of expression tree, but with spaces between nodes
				// i.e. we let the back-end generator sort this stuff out
				auto expressionState = stateMachine->GetPatternInsertionWithName("expression");
				std::string expressionValue;
				if (expressionState.empty() || expressionState.size() > 1)
				{
					throw std::logic_error("Multiple expressions in pattern\n");
				}
				for (auto node : expressionState[0]->GetNode())
				{
					expressionValue += node->GetText() + " ";
				}
				ir::Expression* expression = new ir::Value(expressionValue);
				auto newConditionalStatement = new ir::ConditionalElseIf(expression, statements);
				functionStatements[functionStatements.size() - 1].push_back(
					newConditionalStatement);
			}
		}

		void ConstructConditionalElse(PatternStateMachine* stateMachine)
		{
			std::cout << "Constructing a conditional else if:\n";
			auto statementStates = stateMachine->GetPatternInsertionWithName("statements");
			if (statementStates.empty())
			{
				std::cout << "\tThere are no statement states!\n";
			}
			for (auto statementState : statementStates)
			{
				bool succes = ParseStatements(statementState);
				auto statements = functionStatements[functionStatements.size() - 1];
				functionStatements.pop_back();

				auto newConditionalStatement = new ir::ConditionalElse(statements);
				functionStatements[functionStatements.size() - 1].push_back(
					newConditionalStatement);
			}
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

		// pop back functionStatements, when you processed the current statements
		bool ParseStatements(PatternStateMachine::State* statements)
		{
			functionStatements.emplace_back();

			std::cout << "\tRetrieving Statements using pattern:\n";
			std::cout << "\t\tPattern direction: "
					  << GetPatternDirection(statements->GetPredefinedFormat().value()) << "\n";
			std::cout << "\t\tPattern insertion: "
					  << GetPatternInsertion(statements->GetPredefinedFormat().value()) << "\n";

			auto functionStatementsTranslator = IRTranslatorFunctionStatements(
				this, GetPatternDirection(statements->GetPredefinedFormat().value()));
			bool success = functionStatementsTranslator.GetStatements(statements->GetNode());

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
