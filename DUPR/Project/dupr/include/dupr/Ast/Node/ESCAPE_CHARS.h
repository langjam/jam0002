#ifndef dupr_AST_NODE_ESCAPE_CHARS_H
#define dupr_AST_NODE_ESCAPE_CHARS_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class ESCAPE_CHARS : public dupr<ESCAPE_CHARS>
	{
	private:
	public:
		ESCAPE_CHARS(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<ESCAPE_CHARS>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_ESCAPE_CHARS_H
