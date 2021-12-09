#ifndef dupr_AST_NODE_ADD_H
#define dupr_AST_NODE_ADD_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class ADD : public dupr<ADD>
	{
	private:
	public:
		ADD(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<ADD>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_ADD_H
