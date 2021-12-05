#include "dupr/Ast/Listener/User/IRTranslator.h"

std::string dupr::ast::listener::user::GetPatternInsertion(std::string stateName)
{
	if (stateName.size() < 5 ||
		((stateName[0] != '[') && (stateName[1] != '[') &&
		 (stateName[stateName.size() - 3] != ']') && (stateName[stateName.size() - 2] != ']')))
	{
		return "";
	}

	stateName.erase(0, 2);
	stateName.pop_back();
	stateName.pop_back();

	if (stateName.find(":") == std::string::npos)
	{
		return stateName;
	}
	stateName.erase(0, stateName.find(":") + 1);
	return stateName;
}

std::string dupr::ast::listener::user::GetPatternDirection(std::string stateName)
{
	if (stateName.size() < 5 ||
		((stateName[0] != '[') && (stateName[1] != '[') &&
		 (stateName[stateName.size() - 3] != ']') && (stateName[stateName.size() - 2] != ']')))
	{
		return {};
	}

	stateName.erase(0, 2);
	stateName.pop_back();
	stateName.pop_back();

	if (stateName.find(":") == std::string::npos)
	{
		return {};
	}
	stateName = stateName.substr(0, stateName.find(":"));

	return {stateName};
}

std::vector<std::string> dupr::ast::listener::user::GetPatternDirections(std::string stateName)
{
	if (stateName.size() < 5 ||
		((stateName[0] != '[') && (stateName[1] != '[') &&
		 (stateName[stateName.size() - 3] != ']') && (stateName[stateName.size() - 2] != ']')))
	{
		return {};
	}

	stateName.erase(0, 2);
	stateName.pop_back();
	stateName.pop_back();

	if (stateName.find(":") == std::string::npos)
	{
		return {};
	}
	stateName = stateName.substr(0, stateName.find(":"));
	if (stateName.find(">") == std::string::npos)
	{
		return {stateName};
	}
	std::vector<std::string> directions;
	while (stateName.find_first_of(">") != std::string::npos)
	{
		directions.push_back(stateName.substr(0, stateName.find_first_of(">")));
		stateName.erase(0, stateName.find_first_of(">") + 1);
	}

	if (!stateName.empty())
		directions.push_back(stateName);

	return directions;
}

bool dupr::ast::listener::user::PatternStateMachine::Check(
	const dupr::ast::node::pattern_constructor_encapsulation* currentNode, std::size_t& index,
	bool execute) const
{
	// assumes the encapsulation exists of 3 parts: (pre) or ([[any_word]])
	// However we need to know where the top part ends in the states, allowing us to go
	// deeper if needed.
	bool valid = true;

	using StateStart = State;
	using StateEnd = State;
	std::vector<StateStart*> encapsulationStartStates;
	std::vector<StateEnd*> encapsulationEndStates;
	if (!(GetCurrentState(index)->GetType() == StateType::predefined_formatter &&
		  GetCurrentState(index)->GetPredefinedFormat().value() ==
			  currentNode->GetIndex(0)->GetText()))
	{
		return false;
	}
	std::size_t difference = 0;
	std::size_t state_index = index;
	// Get corresponding ending state
	// If underlying states are found, map them
	for (; state_index < states.size(); state_index++)
	{
		if (GetCurrentState(state_index)->IsStartOfEncapsulation())
		{
			difference += 1;
			encapsulationStartStates.push_back(GetCurrentState(state_index));
		}

		if (GetCurrentState(state_index)->IsEndOfEncapsulation())
		{
			difference -= 1;
			encapsulationEndStates.push_back(GetCurrentState(state_index));
		}

		if (difference == 0) // We are out of the original encapsulation.
		{
			break;
		}
	}
	if (encapsulationStartStates.size() != encapsulationEndStates.size())
	{
		return false;
	}

	const std::size_t encapsulationStates = state_index - index;

	if (currentNode->GetIndex(0)->GetText() != GetCurrentState(index)->GetPredefinedFormat())
	{
		return false;
	}
	index += 1;

	const auto subStatements = GetAccessor(currentNode)
								   .pattern_constructor_content()
								   .pattern_constructor_content_stmt()
								   .GetContent();
	if (GetCurrentState(index)->GetType() == StateType::expression_tree && subStatements.empty())
	{
		index += 2;
		return true;
	}

	for (std::size_t statement_index = 0; statement_index < subStatements.size(); statement_index++)
	{
		const auto subStatement = subStatements[statement_index];
		const auto statement = subStatement->GetIndex(0);
		auto currentState = GetCurrentState(index);
		// If remaining belongs to an expression_tree type, add everything to tree and end.
		// Otherwise the content is in some way predetermined, and we should look through it.
		if (currentState->GetType() == StateType::expression_tree)
		{
			for (std::size_t statement_index_remaining = statement_index;
				 statement_index_remaining < subStatements.size(); statement_index_remaining++)
			{
				const auto subStatementRemaining = subStatements[statement_index_remaining];
				if (execute)
				{
					currentState->AddNode(subStatementRemaining);
				}
			}
			index += 2;
			return true;
		}

		switch (static_cast<dupr::ast::Type>(statement->GetType()))
		{
		case dupr::ast::Type::pattern_constructor_operator: {
			valid =
				Check(static_cast<const dupr::ast::node::pattern_constructor_operator*>(statement),
					  index, execute);
			if (!valid)
			{
				return false;
			}
			break;
		}
		case dupr::ast::Type::pattern_constructor_terminate: {
			valid =
				Check(static_cast<const dupr::ast::node::pattern_constructor_terminate*>(statement),
					  index, execute);
			if (!valid)
			{
				return false;
			}
			break;
		}
		case dupr::ast::Type::pattern_constructor_structure: {
			valid =
				Check(static_cast<const dupr::ast::node::pattern_constructor_structure*>(statement),
					  index, execute);
			if (!valid)
			{
				return false;
			}
			break;
		}
		case dupr::ast::Type::pattern_constructor_encapsulation: {
			valid = Check(
				static_cast<const dupr::ast::node::pattern_constructor_encapsulation*>(statement),
				index, execute);
			if (!valid)
			{
				return false;
			}
			break;
		}
		default: {
			throw std::logic_error("The tree is invalid, OR the grammar has been updated.");
		}
		}
	}

	if (currentNode->GetIndex(2)->GetText() != GetCurrentState(index)->GetPredefinedFormat())
	{
		return false;
	}
	index += 1;

	return true;
}
