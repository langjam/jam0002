#ifndef dupr_AST_NODE_DOT_H
#define dupr_AST_NODE_DOT_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class DOT : public dupr<DOT>
	{
	private:
	public:
		DOT(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<DOT>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_DOT_H
