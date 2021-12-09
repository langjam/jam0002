#ifndef dupr_AST_NODE_pattern_type_H
#define dupr_AST_NODE_pattern_type_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node { 

	class pattern_type : public dupr<pattern_type>
	{
	private:
	public:
		pattern_type(deamer::external::cpp::ast::NodeInformation information_, std::vector<deamer::external::cpp::ast::Node*> nodes_)
		: dupr<pattern_type>(information_, nodes_, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_pattern_type_H
