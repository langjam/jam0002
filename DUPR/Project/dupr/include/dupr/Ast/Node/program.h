#ifndef dupr_AST_NODE_program_H
#define dupr_AST_NODE_program_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node { 

	class program : public dupr<program>
	{
	private:
	public:
		program(deamer::external::cpp::ast::NodeInformation information_, std::vector<deamer::external::cpp::ast::Node*> nodes_)
		: dupr<program>(information_, nodes_, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_program_H
