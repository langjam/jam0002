#ifndef dupr_AST_NODE_stmt_H
#define dupr_AST_NODE_stmt_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node { 

	class stmt : public dupr<stmt>
	{
	private:
	public:
		stmt(deamer::external::cpp::ast::NodeInformation information_, std::vector<deamer::external::cpp::ast::Node*> nodes_)
		: dupr<stmt>(information_, nodes_, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_stmt_H
