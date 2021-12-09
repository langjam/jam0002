#ifndef dupr_AST_NODE_ANDAND_H
#define dupr_AST_NODE_ANDAND_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class ANDAND : public dupr<ANDAND>
	{
	private:
	public:
		ANDAND(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<ANDAND>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_ANDAND_H
