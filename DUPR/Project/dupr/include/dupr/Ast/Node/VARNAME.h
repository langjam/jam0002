#ifndef dupr_AST_NODE_VARNAME_H
#define dupr_AST_NODE_VARNAME_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class VARNAME : public dupr<VARNAME>
	{
	private:
	public:
		VARNAME(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<VARNAME>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_VARNAME_H
