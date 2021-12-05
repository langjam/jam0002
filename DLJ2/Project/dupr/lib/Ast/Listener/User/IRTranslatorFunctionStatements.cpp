#include "dupr/Ast/Listener/User/IRTranslatorFunctionStatements.h"
#include "dupr/Ast/Listener/User/IRTranslator.h"

dupr::ast::listener::user::IRTranslatorFunctionStatements::IRTranslatorFunctionStatements(
	IRTranslator* irTranslator_, std::string patternDirection_)
	: irTranslator(irTranslator_),
	  patternDirection(patternDirection_)
{
}

bool dupr::ast::listener::user::IRTranslatorFunctionStatements::GetStatements(
	std::vector<const ::deamer::external::cpp::ast::Node*> nodes)
{
	bool success = false;

	std::size_t tokensAdded = 0;
	std::vector<const ::deamer::external::cpp::ast::Node*> currentTokens;

	content = nullptr;
	statements.clear();
	for (auto node : nodes)
	{
		Dispatch(node);
	}

	auto statementStateMachines = GetAllStatementStatemachines(patternDirection);
	for (auto stateMachine : statementStateMachines)
	{
		std::cout << "Name: " + stateMachine->GetName() + " Type: " + stateMachine->GetType() +
						 "\n";
	}
	while (true)
	{
		if (tokensAdded < statements.size())
		{
			currentTokens.push_back(statements[tokensAdded]);
			tokensAdded += 1;
		}
		else
		{
			break;
		}
	}

	return false;
}

// Loops in the member: mapDirectionWithPatterns Cause infinite loops
std::vector<dupr::ast::listener::user::PatternStateMachine*>
dupr::ast::listener::user::IRTranslatorFunctionStatements::GetAllStatementStatemachines(
	const std::string& pattern)
{
	std::vector<dupr::ast::listener::user::PatternStateMachine*> stateMachines;

	auto iter = irTranslator->mapDirectionWithPatterns.find(pattern + ">");
	if (iter == irTranslator->mapDirectionWithPatterns.end())
	{
		auto iter2 = irTranslator->mapNameWithPattern.find(pattern);
		if (iter2 == irTranslator->mapNameWithPattern.end())
		{
			// Statemachine points to invalid pattern, more will go wrong
			return {};
		}
		return iter2->second;
	}

	for (auto subPattern : iter->second)
	{
		for (auto stateMachine : GetAllStatementStatemachines(subPattern))
		{
			stateMachines.push_back(stateMachine);
		}
	}

	return stateMachines;
}
