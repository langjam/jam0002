#ifndef dupr_AST_NODE_deamerreserved_star__stmt___H
#define dupr_AST_NODE_deamerreserved_star__stmt___H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node { 

	class deamerreserved_star__stmt__ : public dupr<deamerreserved_star__stmt__>
	{
	private:
	public:
		deamerreserved_star__stmt__(deamer::external::cpp::ast::NodeInformation information_, std::vector<deamer::external::cpp::ast::Node*> nodes_)
		: dupr<deamerreserved_star__stmt__>(information_, nodes_, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_deamerreserved_star__stmt___H
