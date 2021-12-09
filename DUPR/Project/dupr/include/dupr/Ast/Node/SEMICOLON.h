#ifndef dupr_AST_NODE_SEMICOLON_H
#define dupr_AST_NODE_SEMICOLON_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class SEMICOLON : public dupr<SEMICOLON>
	{
	private:
	public:
		SEMICOLON(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<SEMICOLON>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_SEMICOLON_H
