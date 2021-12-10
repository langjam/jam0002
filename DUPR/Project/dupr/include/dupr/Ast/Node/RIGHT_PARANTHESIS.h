#ifndef dupr_AST_NODE_RIGHT_PARANTHESIS_H
#define dupr_AST_NODE_RIGHT_PARANTHESIS_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class RIGHT_PARANTHESIS : public dupr<RIGHT_PARANTHESIS>
	{
	private:
	public:
		RIGHT_PARANTHESIS(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<RIGHT_PARANTHESIS>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_RIGHT_PARANTHESIS_H
