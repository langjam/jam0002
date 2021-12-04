#ifndef dupr_AST_NODE_pattern_constructor_nt_content_H
#define dupr_AST_NODE_pattern_constructor_nt_content_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node { 

	class pattern_constructor_nt_content : public dupr<pattern_constructor_nt_content>
	{
	private:
	public:
		pattern_constructor_nt_content(deamer::external::cpp::ast::NodeInformation information_, std::vector<deamer::external::cpp::ast::Node*> nodes_)
		: dupr<pattern_constructor_nt_content>(information_, nodes_, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_pattern_constructor_nt_content_H
