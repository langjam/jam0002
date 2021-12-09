#ifndef dupr_AST_NODE_LE_H
#define dupr_AST_NODE_LE_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class LE : public dupr<LE>
	{
	private:
	public:
		LE(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<LE>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_LE_H
