#ifndef dupr_AST_NODE_pattern_name_H
#define dupr_AST_NODE_pattern_name_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node { 

	class pattern_name : public dupr<pattern_name>
	{
	private:
	public:
		pattern_name(deamer::external::cpp::ast::NodeInformation information_, std::vector<deamer::external::cpp::ast::Node*> nodes_)
		: dupr<pattern_name>(information_, nodes_, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_pattern_name_H
