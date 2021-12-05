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

void dupr::ast::listener::user::PatternStateMachine::AddCallback(
	std::function<void(PatternStateMachine*)> callback_)
{
	callbacks.push_back(callback_);
}

bool dupr::ast::listener::user::PatternStateMachine::IsEmpty() const
{
	return states.empty();
}

dupr::ast::listener::user::PatternStateMachine::~PatternStateMachine()
{
	for (auto* state : states)
	{
		delete state;
	}
}

std::string dupr::ast::listener::user::PatternStateMachine::GetType() const
{
	return type;
}

std::string dupr::ast::listener::user::PatternStateMachine::GetName() const
{
	return name;
}

std::vector<dupr::ast::listener::user::PatternStateMachine::State*>
dupr::ast::listener::user::PatternStateMachine::GetStates() const
{
	return states;
}

bool dupr::ast::listener::user::PatternStateMachine::Check(
	const std::vector<const ::deamer::external::cpp::ast::Node*>& currentNodes, bool execute,
	bool print)
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
									 GetCurrentState(index)->GetPredefinedFormat().value() + "\n";
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
									 GetCurrentState(index)->GetPredefinedFormat().value() + "\n";
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
									 GetCurrentState(index)->GetPredefinedFormat().value() + "\n";
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

void dupr::ast::listener::user::PatternStateMachine::Execute(
	const std::vector<const ::deamer::external::cpp::ast::Node*>& currentNodes)
{
	ResetNodesOfStates();
	Check(currentNodes, true, false);
	for (auto callback : callbacks)
	{
		callback(this);
	}
}

bool dupr::ast::listener::user::PatternStateMachine::Check(
	const dupr::ast::node::pattern_constructor_content* currentNode, bool execute, bool print)
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
		std::cout << "\tParsing (ignoring escape chars): " << currentNode->GetText() << "\n";
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
			const auto* terminatingNT = statement.pattern_constructor_operator().GetContent()[0];
			const bool valid = Check(terminatingNT, index, execute);

			if (!valid)
			{
				if (print)
				{
					std::cout << "\tPattern failed at '" + terminatingNT->GetText() +
									 "': operator, expected: " +
									 GetCurrentState(index)->GetPredefinedFormat().value() + "\n";
				}
				return false;
			}
		}
		if (!statement.pattern_constructor_terminate().GetContent().empty())
		{
			const auto* terminatingNT = statement.pattern_constructor_terminate().GetContent()[0];
			const bool valid = Check(terminatingNT, index, execute);

			if (!valid)
			{
				if (print)
				{
					std::cout << "\tPattern failed at '" + terminatingNT->GetText() +
									 "': termination, expected: " +
									 GetCurrentState(index)->GetPredefinedFormat().value() + "\n";
				}
				return false;
			}
		}
		if (!statement.pattern_constructor_structure().GetContent().empty())
		{
			const auto* terminatingNT = statement.pattern_constructor_structure().GetContent()[0];
			const bool valid = Check(terminatingNT, index, execute);

			if (!valid)
			{
				if (print)
				{
					std::cout << "\tPattern failed at '" + terminatingNT->GetText() +
									 "': structure, expected: " +
									 GetCurrentState(index)->GetPredefinedFormat().value() + "\n";
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

bool dupr::ast::listener::user::PatternStateMachine::Check(
	const dupr::ast::node::pattern_constructor_encapsulation* currentNode, std::size_t& index,
	bool execute)
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
		auto currentState = GetCurrentState(index);
		auto nextState = GetNextState(index);
		// If remaining belongs to an expression_tree type, add everything to tree and end.
		// Otherwise the content is in some way predetermined, and we should look through it.
		if (currentState->GetType() == StateType::expression_tree)
		{
			// Make sure the next state is not the same state, and not an expression tree.
			if (nextState.has_value() && nextState.value()->GetType() == StateType::expression_tree)
			{
				while (nextState.has_value() && currentState->IsStateTheSame(nextState.value()))
				{
					states.erase(states.begin() + index);
					nextState = GetNextState(index);
				}

				if (nextState.has_value() &&
					nextState.value()->GetType() == StateType::expression_tree)
				{
					return false;
				}
			}
			bool encounterPredefined = false;
			for (std::size_t statement_index_remaining = statement_index;
				 statement_index_remaining < subStatements.size(); statement_index_remaining++)
			{
				const auto subStatementRemaining = subStatements[statement_index_remaining];
				if (nextState.has_value())
				{
					if (nextState.value()->GetPredefinedFormat().value() ==
							subStatementRemaining->GetText() ||
						nextState.value()->GetPredefinedFormat().value() ==
							subStatementRemaining->GetIndex(0)->GetIndex(0)->GetText())
					{
						index += 1;
						statement_index = statement_index_remaining;
						encounterPredefined = true;
						break;
					}
				}
				if (execute)
				{
					currentState->AddNode(subStatementRemaining);
				}
			}
			if (!encounterPredefined)
			{
				index += 2;
				return true;
			}
		}

		const auto subStatement = subStatements[statement_index];
		const auto statement = subStatement->GetIndex(0);
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

bool dupr::ast::listener::user::PatternStateMachine::Check(
	const dupr::ast::node::pattern_constructor_operator* currentNode, std::size_t& index,
	bool execute)
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
		if (nextState.has_value() && (nextState.value()->GetType() == StateType::any_word ||
									  nextState.value()->GetType() == StateType::expression_tree))
		{
			std::cout << "Unparsable situation!\n";
			valid = false;
		}
		else if (nextState.has_value() && GetAccessor(currentNode).GetContent()[0]->GetText() ==
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

bool dupr::ast::listener::user::PatternStateMachine::Check(
	const dupr::ast::node::pattern_constructor_structure* currentNode, std::size_t& index,
	bool execute)
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

bool dupr::ast::listener::user::PatternStateMachine::Check(
	const dupr::ast::node::pattern_constructor_terminate* currentNode, std::size_t& index,
	bool execute)
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
		if (nextState.has_value() && (nextState.value()->GetType() == StateType::any_word ||
									  nextState.value()->GetType() == StateType::expression_tree))
		{
			std::cout << "Unparsable situation!\n";
			valid = false;
		}
		else if (nextState.has_value() && GetAccessor(currentNode).GetContent()[0]->GetText() ==
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

dupr::ast::listener::user::PatternStateMachine::State*
dupr::ast::listener::user::PatternStateMachine::GetCurrentState(std::size_t index) const
{
	if (index < states.size())
	{
		return states[index];
	}

	throw std::logic_error("There are no states left.");
}

std::optional<dupr::ast::listener::user::PatternStateMachine::State*>
dupr::ast::listener::user::PatternStateMachine::GetNextState(std::size_t index) const
{
	if ((index + 1) < states.size())
	{
		return states[index + 1];
	}

	return std::nullopt;
}

void dupr::ast::listener::user::PatternStateMachine::ResetNodesOfStates()
{
	for (auto* state : states)
	{
		state->ResetNodes();
	}
}

void dupr::ast::listener::user::PatternStateMachine::Execute(
	const node::pattern_constructor_content* patternConstructorContent)
{
	ResetNodesOfStates();
	Check(patternConstructorContent, true, false);
	for (auto callback : callbacks)
	{
		callback(this);
	}
}

std::vector<dupr::ast::listener::user::PatternStateMachine::State*>
dupr::ast::listener::user::PatternStateMachine::GetStatesWithName(std::string name)
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

std::vector<dupr::ast::listener::user::PatternStateMachine::State*>
dupr::ast::listener::user::PatternStateMachine::GetPatternInsertionWithName(std::string name)
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
