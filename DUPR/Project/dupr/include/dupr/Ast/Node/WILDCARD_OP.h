#ifndef dupr_AST_NODE_WILDCARD_OP_H
#define dupr_AST_NODE_WILDCARD_OP_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class WILDCARD_OP : public dupr<WILDCARD_OP>
	{
	private:
	public:
		WILDCARD_OP(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<WILDCARD_OP>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_WILDCARD_OP_H
