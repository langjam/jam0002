#ifndef dupr_AST_NODE_pattern_execution_content_stmt_H
#define dupr_AST_NODE_pattern_execution_content_stmt_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node { 

	class pattern_execution_content_stmt : public dupr<pattern_execution_content_stmt>
	{
	private:
	public:
		pattern_execution_content_stmt(deamer::external::cpp::ast::NodeInformation information_, std::vector<deamer::external::cpp::ast::Node*> nodes_)
		: dupr<pattern_execution_content_stmt>(information_, nodes_, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_pattern_execution_content_stmt_H
