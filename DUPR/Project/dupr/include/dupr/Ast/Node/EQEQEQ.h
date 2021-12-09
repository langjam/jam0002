#ifndef dupr_AST_NODE_EQEQEQ_H
#define dupr_AST_NODE_EQEQEQ_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class EQEQEQ : public dupr<EQEQEQ>
	{
	private:
	public:
		EQEQEQ(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<EQEQEQ>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_EQEQEQ_H
