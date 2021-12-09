#ifndef dupr_AST_NODE_AND_H
#define dupr_AST_NODE_AND_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class AND : public dupr<AND>
	{
	private:
	public:
		AND(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<AND>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_AND_H
