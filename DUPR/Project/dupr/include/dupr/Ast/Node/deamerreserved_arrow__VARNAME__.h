#ifndef dupr_AST_NODE_deamerreserved_arrow__VARNAME___H
#define dupr_AST_NODE_deamerreserved_arrow__VARNAME___H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node { 

	class deamerreserved_arrow__VARNAME__ : public dupr<deamerreserved_arrow__VARNAME__>
	{
	private:
	public:
		deamerreserved_arrow__VARNAME__(deamer::external::cpp::ast::NodeInformation information_, std::vector<deamer::external::cpp::ast::Node*> nodes_)
		: dupr<deamerreserved_arrow__VARNAME__>(information_, nodes_, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_deamerreserved_arrow__VARNAME___H
