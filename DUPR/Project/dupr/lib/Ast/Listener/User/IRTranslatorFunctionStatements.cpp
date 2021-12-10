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

	content = nullptr;
	statements.clear();
	for (auto node : nodes)
	{
		Dispatch(node);
	}

	auto statementStateMachines = GetAllStatementStatemachines(patternDirection + ">");
	for (auto stateMachine : statementStateMachines)
	{
		std::cout << "Name: " + stateMachine->GetName() + " Type: " + stateMachine->GetType() +
						 "\n";
	}

	std::vector<
		std::pair<PatternStateMachine*, std::vector<const ::deamer::external::cpp::ast::Node*>>>
		remember;

	std::size_t parsedTokens = 0;
	// Continue till all tokens are parsed, or some failure happened
	while (true)
	{
		if (parsedTokens == statements.size())
		{
			break;
		}
		std::size_t tokensAdded = parsedTokens;
		std::vector<const ::deamer::external::cpp::ast::Node*> currentTokens;
		// Deduce all options
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

			for (auto stateMachine : statementStateMachines)
			{
				bool valid = stateMachine->Check(currentTokens, false, false);
				if (valid)
				{
					bool exists = false;
					// Remember statemachine
					for (auto iter = std::begin(remember); iter != std::end(remember); iter++)
					{
						auto stateM = iter->first;
						if (stateM == stateMachine)
						{
							// optionally remove with longer matching token set.
							remember.erase(iter);
							remember.emplace_back(stateMachine, currentTokens);
							exists = true;
							break;
						}
					}
					if (!exists)
					{
						remember.emplace_back(stateMachine, currentTokens);
					}
				}
			}
		}

		bool couldParse = false;
		// Get statemachine with highest precedence, and parse the tokens.
		for (auto stateMachineOrder : statementStateMachines)
		{
			bool found = false;
			std::vector<const ::deamer::external::cpp::ast::Node*> rememberedTokens;
			for (auto [stateMachine, rem] : remember)
			{
				if (stateMachine == stateMachineOrder)
				{
					rememberedTokens = rem;
					found = true;
					break;
				}
			}

			if (found)
			{
				stateMachineOrder->Execute(rememberedTokens);
				std::cout << "\t\tStatement has been deduced: " + stateMachineOrder->GetType() +
								 " " + stateMachineOrder->GetName() + "!\n";
				parsedTokens += rememberedTokens.size();
				remember.clear();
				couldParse = true;
				break;
			}
		}

		if (!couldParse)
		{
			return false;
		}
	}

	return parsedTokens == statements.size();
}

// Loops in the member: mapDirectionWithPatterns Cause infinite loops
std::vector<dupr::ast::listener::user::PatternStateMachine*>
dupr::ast::listener::user::IRTranslatorFunctionStatements::GetAllStatementStatemachines(
	const std::string& pattern)
{
	std::vector<dupr::ast::listener::user::PatternStateMachine*> stateMachines;

	auto iter = irTranslator->mapDirectionWithPatterns.find(pattern);
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
