#ifndef dupr_AST_NODE_RIGHT_BRACKET_H
#define dupr_AST_NODE_RIGHT_BRACKET_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class RIGHT_BRACKET : public dupr<RIGHT_BRACKET>
	{
	private:
	public:
		RIGHT_BRACKET(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<RIGHT_BRACKET>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_RIGHT_BRACKET_H
