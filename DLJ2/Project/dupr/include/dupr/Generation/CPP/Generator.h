#ifndef DUPR_GENERATION_CPP_GENERATOR_H
#define DUPR_GENERATION_CPP_GENERATOR_H

#include "dupr/Generation/CPP/Templates/CPPTemplate.h"
#include "dupr/Generation/CPP/Templates/StatementTemplate.h"
#include "dupr/IR/ConditionalElse.h"
#include "dupr/IR/ConditionalElseIf.h"
#include "dupr/IR/ConditionalIf.h"
#include "dupr/IR/Function.h"
#include "dupr/IR/ReturnStatement.h"
#include "dupr/IR/Table.h"
#include "dupr/IR/VariableAssignment.h"
#include "dupr/IR/VariableDeclaration.h"
#include <iostream>
#include <memory>
#include <string>

namespace dupr::generation::cpp
{
	class Generator
	{
	private:
		ir::Table* table;
		std::vector<std::unique_ptr<templates::StatementTemplate>> statementTemplates;

	public:
		Generator(ir::Table* table_) : table(table_)
		{
		}

	public:
		// Generate C++ code from IR.
		std::string Generate()
		{
			statementTemplates.clear();

			auto cppTemplate = templates::CPPTemplate();
			for (auto function : table->Get(ir::OrderType::Function))
			{
				cppTemplate.argument_->variable_field_->Clear();
				cppTemplate.statement_->variable_field_->Clear();

				auto returnType = static_cast<ir::Function*>(function)->GetReturnType();
				auto name = static_cast<ir::Function*>(function)->GetFunctionName();
				auto arguments = static_cast<ir::Function*>(function)->GetFunctionArguments();
				auto statements = static_cast<ir::Function*>(function)->GetStatements();

				cppTemplate.return_type_->Set(returnType);
				cppTemplate.function_name_->Set(name);

				for (const auto& argument : arguments)
				{
					cppTemplate.argument_type_->Set(argument.GetType());
					cppTemplate.argument_name_->Set(argument.GetName());

					cppTemplate.argument_->ExpandVariableField();
				}

				for (auto statement : statements)
				{
					auto statementTemplate = GetStatementTemplate();

					ParseStatement(statement, statementTemplate);

					cppTemplate.statement_->Set(statementTemplate->GetOutput());
					cppTemplate.statement_->ExpandVariableField();
				}

				cppTemplate.function_->ExpandVariableField();
				cppTemplate.function_prototype_->ExpandVariableField();
			}

			return cppTemplate.GetOutput();
		}

		templates::StatementTemplate* GetStatementTemplate()
		{
			auto newTemplate = std::make_unique<templates::StatementTemplate>();
			const auto returnValue = newTemplate.get();

			statementTemplates.push_back(std::move(newTemplate));

			return returnValue;
		}

		void ParseStatement(ir::Statement* value, templates::StatementTemplate* statementTemplate)
		{
			switch (value->GetType())
			{
			case ir::Statement::Type::ReturnStatement: {
				statementTemplate->expression_->Set(
					static_cast<ir::ReturnStatement*>(value)->GetExpression()->GetExpressionText());
				statementTemplate->statement_->Set(statementTemplate->return_statement_);
				return;
			}
			case ir::Statement::Type::VariableDeclaration: {
				statementTemplate->var_name_->Set(
					static_cast<ir::VariableDeclaration*>(value)->GetVariableName());
				statementTemplate->var_type_->Set(
					static_cast<ir::VariableDeclaration*>(value)->GetVariableType());
				statementTemplate->statement_->Set(statementTemplate->variable_declaration_);
				return;
			}
			case ir::Statement::Type::VariableAssignment: {
				statementTemplate->var_name_->Set(
					static_cast<ir::VariableAssignment*>(value)->GetVariableName());
				statementTemplate->expression_->Set(static_cast<ir::VariableAssignment*>(value)
														->GetExpression()
														->GetExpressionText());
				statementTemplate->statement_->Set(statementTemplate->variable_assignment_);
				return;
			}
			case ir::Statement::Type::VariableInitialization: {
				std::cout << "VariableInitialization is unsupported\n";
				return;
			}
			case ir::Statement::Type::ConditionalIf: {
				statementTemplate->expression_->Set(
					static_cast<ir::ConditionalIf*>(value)->GetExpression()->GetExpressionText());

				for (auto statement : static_cast<ir::ConditionalIf*>(value)->GetStatements())
				{
					auto statementSubTemplate = GetStatementTemplate();
					ParseStatement(statement, statementSubTemplate);
					statementTemplate->external_statement_->Set(statementSubTemplate->GetOutput());
					statementTemplate->external_statement_->ExpandVariableField();
				}
				statementTemplate->statement_->Set(statementTemplate->conditional_if_);
				return;
			}
			case ir::Statement::Type::ConditionalElseIf: {
				statementTemplate->expression_->Set(static_cast<ir::ConditionalElseIf*>(value)
														->GetExpression()
														->GetExpressionText());

				for (auto statement : static_cast<ir::ConditionalElseIf*>(value)->GetStatements())
				{
					auto statementSubTemplate = GetStatementTemplate();
					ParseStatement(statement, statementSubTemplate);
					statementTemplate->external_statement_->Set(statementSubTemplate->GetOutput());
					statementTemplate->external_statement_->ExpandVariableField();
				}
				statementTemplate->statement_->Set(statementTemplate->conditional_else_if_);
				return;
			}
			case ir::Statement::Type::ConditionalElse: {
				for (auto statement : static_cast<ir::ConditionalElse*>(value)->GetStatements())
				{
					auto statementSubTemplate = GetStatementTemplate();
					ParseStatement(statement, statementSubTemplate);
					statementTemplate->external_statement_->Set(statementSubTemplate->GetOutput());
					statementTemplate->external_statement_->ExpandVariableField();
				}
				statementTemplate->statement_->Set(statementTemplate->conditional_else_);
				return;
			}
			}

			std::cout << "CPP Generator: Unknown type given!\n";
			return;
		}
	};
}

#endif // DUPR_GENERATION_CPP_GENERATOR_H
