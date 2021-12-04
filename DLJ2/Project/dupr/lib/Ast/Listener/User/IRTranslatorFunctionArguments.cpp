#include "dupr/Ast/Listener/User/IRTranslatorFunctionArguments.h"
#include "dupr/Ast/Listener/User/IRTranslator.h"

bool dupr::ast::listener::user::IRTranslatorFunctionArguments::GetArguments(
	IRTranslator* irTranslator, std::string patternDirection,
	std::vector<const ::deamer::external::cpp::ast::Node*> nodes)
{
	bool success = false;

	std::size_t tokensAdded = 0;
	bool argumentOrExtension = false; // false: argument statemachine, true: extension statemachine
	std::vector<const ::deamer::external::cpp::ast::Node*> currentTokens;

	content = nullptr;
	statements.clear();
	for (auto node : nodes)
	{
		Dispatch(node);
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

		if (argumentOrExtension)
		{
			bool valid =
				ParseExtension(currentTokens, irTranslator, patternDirection + ">Extension");
			if (valid)
			{
				currentTokens.clear();
				argumentOrExtension = false;
			}
		}
		else
		{
			bool valid = ParseArgument(currentTokens, irTranslator, patternDirection + ">Argument");
			if (valid)
			{
				currentTokens.clear();
				argumentOrExtension = true;
			}
		}
	}

	success = tokensAdded == statements.size() && currentTokens.empty();
	if (success)
	{
		std::cout << "\t\tGiven input has been correctly parsed!\n";
	}
	else
	{
		std::cout << "\t\tGiven input node cannot be parsed by argument patterns.\n";
	}

	return success;
}

bool dupr::ast::listener::user::IRTranslatorFunctionArguments::ParseExtension(
	const std::vector<const deamer::external::cpp::ast::Node*>& nodes, IRTranslator* irTranslator,
	const std::string& patternDirection)
{
	auto iter = irTranslator->mapNameWithPattern.find(patternDirection);
	if (iter == irTranslator->mapNameWithPattern.end())
	{
		throw std::logic_error(
			"ArgumentExtension pattern direction does not exist: " + patternDirection + "\n");
	}

	bool valid = false;
	for (auto stateMachine : iter->second)
	{
		valid |= stateMachine->Check(nodes);
		if (valid)
		{
			std::cout << "\t\tPattern works!\n";
			stateMachine->Execute(nodes);
			return true;
		}
	}

	return false;
}

bool dupr::ast::listener::user::IRTranslatorFunctionArguments::ParseArgument(
	const std::vector<const deamer::external::cpp::ast::Node*>& nodes, IRTranslator* irTranslator,
	const std::string& patternDirection)
{
	auto iter = irTranslator->mapNameWithPattern.find(patternDirection);
	if (iter == irTranslator->mapNameWithPattern.end())
	{
		throw std::logic_error("Argument pattern direction does not exist: " + patternDirection +
							   "\n");
	}

	bool valid = false;
	for (auto stateMachine : iter->second)
	{
		valid |= stateMachine->Check(nodes);
		if (valid)
		{
			std::cout << "\t\tPattern works!\n";
			stateMachine->Execute(nodes);
			return true;
		}
	}

	return false;
}

bool dupr::ast::listener::user::IRTranslatorFunctionArguments::IsLeftPositional(
	const std::string& cs)
{
	return (cs == "(" || cs == "[" || cs == "{");
}

bool dupr::ast::listener::user::IRTranslatorFunctionArguments::IsRightPositional(
	const std::string& cs)
{
	return (cs == ")" || cs == "]" || cs == "}");
}
