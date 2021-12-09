#ifndef dupr_AST_NODE_COLON_H
#define dupr_AST_NODE_COLON_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class COLON : public dupr<COLON>
	{
	private:
	public:
		COLON(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<COLON>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_COLON_H
