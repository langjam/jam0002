#ifndef dupr_AST_NODE_LEFT_BRACKET_H
#define dupr_AST_NODE_LEFT_BRACKET_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class LEFT_BRACKET : public dupr<LEFT_BRACKET>
	{
	private:
	public:
		LEFT_BRACKET(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<LEFT_BRACKET>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_LEFT_BRACKET_H
