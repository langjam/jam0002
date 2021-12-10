#ifndef dupr_AST_NODE_LT_H
#define dupr_AST_NODE_LT_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class LT : public dupr<LT>
	{
	private:
	public:
		LT(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<LT>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_LT_H
