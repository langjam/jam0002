#ifndef dupr_AST_NODE_DECIMAL_H
#define dupr_AST_NODE_DECIMAL_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class DECIMAL : public dupr<DECIMAL>
	{
	private:
	public:
		DECIMAL(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<DECIMAL>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_DECIMAL_H
