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
