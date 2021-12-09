#ifndef dupr_AST_NODE_pattern_constructor_terminate_H
#define dupr_AST_NODE_pattern_constructor_terminate_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node { 

	class pattern_constructor_terminate : public dupr<pattern_constructor_terminate>
	{
	private:
	public:
		pattern_constructor_terminate(deamer::external::cpp::ast::NodeInformation information_, std::vector<deamer::external::cpp::ast::Node*> nodes_)
		: dupr<pattern_constructor_terminate>(information_, nodes_, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_pattern_constructor_terminate_H
