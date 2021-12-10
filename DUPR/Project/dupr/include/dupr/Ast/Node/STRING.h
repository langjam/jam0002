#ifndef dupr_AST_NODE_STRING_H
#define dupr_AST_NODE_STRING_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class STRING : public dupr<STRING>
	{
	private:
	public:
		STRING(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<STRING>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_STRING_H
