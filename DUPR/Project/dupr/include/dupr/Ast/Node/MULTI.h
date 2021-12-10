#ifndef dupr_AST_NODE_MULTI_H
#define dupr_AST_NODE_MULTI_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class MULTI : public dupr<MULTI>
	{
	private:
	public:
		MULTI(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<MULTI>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_MULTI_H
