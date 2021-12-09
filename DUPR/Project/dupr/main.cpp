#include "dupr/Ast/Listener/User/IRTranslator.h"
#include "dupr/Ast/Visualisation/Graph.h"
#include "dupr/Bison/Parser.h"
#include "dupr/Flex/Lexer.h"
#include "dupr/Generation/CPP/Generator.h"
#include <fstream>
#include <iostream>
#include <memory>
#include <sstream>
#include <vector>

std::string ReadInFile(const std::string& file)
{
	const std::ifstream inputFile(file);

	std::ostringstream sstr;
	sstr << inputFile.rdbuf();

	std::string input = sstr.str();

	return input;
}

int main(int argc, const char* argv[])
{
	std::vector<std::string> filenames = {"./test.txt"};

	for (auto i = 1; i < argc; i++)
	{
		filenames.push_back(argv[1]);
	}

	for (const auto& filename : filenames)
	{
		std::string text = ReadInFile(filename);

		std::cout << "File: " << filename << "\n";
		std::cout << text << "\n";
		std::cout << "\n";

		auto lexer = dupr::lexer::Lexer();

		auto parser = dupr::parser::Parser();
		auto ast = std::unique_ptr<::deamer::external::cpp::ast::Tree>(parser.Parse(text));
		if (ast == nullptr)
		{
			std::cout << "Compilation failed!\n";
			return -1;
		}

		if (ast->GetStartNode() == nullptr)
		{
			std::cout << "Compilation failed!\n";
			return -1;
		}

		auto irTranslator = dupr::ast::listener::user::IRTranslator();
		irTranslator.Dispatch(ast->GetStartNode());

		auto table = irTranslator.GetTable();
		for (auto function : table->Get(dupr::ir::OrderType::Function))
		{
			function->Print();
		}

		auto generator = dupr::generation::cpp::Generator(table.get());
		std::cout << generator.Generate() << "\n";
		if (false)
		{
			auto graph = dupr::ast::listener::deamer::visualisation::Graph();
			graph.Dispatch(ast->GetStartNode());
			std::cout << graph.GetGraph() << std::endl;
		}
	}

	std::cout << "Compilation succeeded!\n";
	return 0;
}
