#ifndef dupr_AST_NODE_LEFT_PARANTHESIS_H
#define dupr_AST_NODE_LEFT_PARANTHESIS_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class LEFT_PARANTHESIS : public dupr<LEFT_PARANTHESIS>
	{
	private:
	public:
		LEFT_PARANTHESIS(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<LEFT_PARANTHESIS>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_LEFT_PARANTHESIS_H
