#ifndef dupr_AST_NODE_GE_H
#define dupr_AST_NODE_GE_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class GE : public dupr<GE>
	{
	private:
	public:
		GE(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<GE>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_GE_H
