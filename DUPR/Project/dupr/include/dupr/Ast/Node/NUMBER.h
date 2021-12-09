#ifndef dupr_AST_NODE_NUMBER_H
#define dupr_AST_NODE_NUMBER_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class NUMBER : public dupr<NUMBER>
	{
	private:
	public:
		NUMBER(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<NUMBER>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_NUMBER_H
