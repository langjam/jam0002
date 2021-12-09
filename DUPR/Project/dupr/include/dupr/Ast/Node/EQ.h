#ifndef dupr_AST_NODE_EQ_H
#define dupr_AST_NODE_EQ_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class EQ : public dupr<EQ>
	{
	private:
	public:
		EQ(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<EQ>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_EQ_H
