#ifndef DUPR_IR_VARIABLEDECLARATION_H
#define DUPR_IR_VARIABLEDECLARATION_H

#include "dupr/IR/Statement.h"
#include <string>
namespace dupr::ir
{
	class VariableDeclaration : public Statement
	{
	private:
		std::string type;
		std::string name;

	public:
		VariableDeclaration(const std::string& type_, const std::string& name_)
			: Statement(Statement::Type::VariableDeclaration),
			  type(type_),
			  name(name_)
		{
		}

	public:
	};
}

#endif // DUPR_IR_VARIABLEDECLARATION_H
