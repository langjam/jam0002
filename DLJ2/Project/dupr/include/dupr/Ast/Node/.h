#ifndef dupr_AST_NODE__H
#define dupr_AST_NODE__H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node { 

	class  : public dupr<>
	{
	private:
	public:
		(deamer::external::cpp::ast::NodeInformation information_, std::vector<deamer::external::cpp::ast::Node*> nodes_)
		: dupr<>(information_, nodes_, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE__H
