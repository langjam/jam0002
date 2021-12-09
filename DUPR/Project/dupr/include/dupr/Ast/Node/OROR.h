#ifndef dupr_AST_NODE_OROR_H
#define dupr_AST_NODE_OROR_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class OROR : public dupr<OROR>
	{
	private:
	public:
		OROR(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<OROR>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_OROR_H
