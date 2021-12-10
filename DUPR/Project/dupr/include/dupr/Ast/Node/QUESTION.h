#ifndef dupr_AST_NODE_QUESTION_H
#define dupr_AST_NODE_QUESTION_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class QUESTION : public dupr<QUESTION>
	{
	private:
	public:
		QUESTION(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<QUESTION>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_QUESTION_H
