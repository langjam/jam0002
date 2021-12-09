#ifndef dupr_AST_NODE_pattern_constructor_line_H
#define dupr_AST_NODE_pattern_constructor_line_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node { 

	class pattern_constructor_line : public dupr<pattern_constructor_line>
	{
	private:
	public:
		pattern_constructor_line(deamer::external::cpp::ast::NodeInformation information_, std::vector<deamer::external::cpp::ast::Node*> nodes_)
		: dupr<pattern_constructor_line>(information_, nodes_, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_pattern_constructor_line_H
