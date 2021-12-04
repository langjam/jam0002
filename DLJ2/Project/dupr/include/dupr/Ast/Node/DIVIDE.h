#ifndef dupr_AST_NODE_DIVIDE_H
#define dupr_AST_NODE_DIVIDE_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class DIVIDE : public dupr<DIVIDE>
	{
	private:
	public:
		DIVIDE(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<DIVIDE>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_DIVIDE_H
