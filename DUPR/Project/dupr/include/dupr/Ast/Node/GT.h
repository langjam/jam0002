#ifndef dupr_AST_NODE_GT_H
#define dupr_AST_NODE_GT_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class GT : public dupr<GT>
	{
	private:
	public:
		GT(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<GT>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_GT_H
