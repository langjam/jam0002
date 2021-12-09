#ifndef dupr_AST_NODE_HEKJE_H
#define dupr_AST_NODE_HEKJE_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class HEKJE : public dupr<HEKJE>
	{
	private:
	public:
		HEKJE(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<HEKJE>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_HEKJE_H
