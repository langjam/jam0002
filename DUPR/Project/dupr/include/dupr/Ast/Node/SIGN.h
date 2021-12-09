#ifndef dupr_AST_NODE_SIGN_H
#define dupr_AST_NODE_SIGN_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class SIGN : public dupr<SIGN>
	{
	private:
	public:
		SIGN(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<SIGN>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_SIGN_H
