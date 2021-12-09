#ifndef dupr_AST_NODE_MINUS_H
#define dupr_AST_NODE_MINUS_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class MINUS : public dupr<MINUS>
	{
	private:
	public:
		MINUS(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<MINUS>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_MINUS_H
