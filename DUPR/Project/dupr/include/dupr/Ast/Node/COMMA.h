#ifndef dupr_AST_NODE_COMMA_H
#define dupr_AST_NODE_COMMA_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class COMMA : public dupr<COMMA>
	{
	private:
	public:
		COMMA(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<COMMA>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_COMMA_H
