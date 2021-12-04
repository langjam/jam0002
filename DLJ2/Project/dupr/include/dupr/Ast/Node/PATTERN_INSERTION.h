#ifndef dupr_AST_NODE_PATTERN_INSERTION_H
#define dupr_AST_NODE_PATTERN_INSERTION_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class PATTERN_INSERTION : public dupr<PATTERN_INSERTION>
	{
	private:
	public:
		PATTERN_INSERTION(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<PATTERN_INSERTION>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_PATTERN_INSERTION_H
