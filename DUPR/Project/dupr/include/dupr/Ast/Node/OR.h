#ifndef dupr_AST_NODE_OR_H
#define dupr_AST_NODE_OR_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class OR : public dupr<OR>
	{
	private:
	public:
		OR(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<OR>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_OR_H
