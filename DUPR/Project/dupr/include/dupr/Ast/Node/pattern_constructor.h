#ifndef dupr_AST_NODE_pattern_constructor_H
#define dupr_AST_NODE_pattern_constructor_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node { 

	class pattern_constructor : public dupr<pattern_constructor>
	{
	private:
	public:
		pattern_constructor(deamer::external::cpp::ast::NodeInformation information_, std::vector<deamer::external::cpp::ast::Node*> nodes_)
		: dupr<pattern_constructor>(information_, nodes_, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_pattern_constructor_H
