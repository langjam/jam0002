#ifndef dupr_AST_NODE_EQEQ_H
#define dupr_AST_NODE_EQEQ_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class EQEQ : public dupr<EQEQ>
	{
	private:
	public:
		EQEQ(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<EQEQ>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_EQEQ_H
