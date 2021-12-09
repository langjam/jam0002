#ifndef dupr_AST_NODE_deamerreserved_plus__pattern_constructor___H
#define dupr_AST_NODE_deamerreserved_plus__pattern_constructor___H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node { 

	class deamerreserved_plus__pattern_constructor__ : public dupr<deamerreserved_plus__pattern_constructor__>
	{
	private:
	public:
		deamerreserved_plus__pattern_constructor__(deamer::external::cpp::ast::NodeInformation information_, std::vector<deamer::external::cpp::ast::Node*> nodes_)
		: dupr<deamerreserved_plus__pattern_constructor__>(information_, nodes_, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_deamerreserved_plus__pattern_constructor___H
