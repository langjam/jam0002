#ifndef dupr_AST_NODE_EXCLAM_H
#define dupr_AST_NODE_EXCLAM_H

#include "dupr/Ast/Node/dupr.h"
#include <Deamer/External/Cpp/Ast/Node.h>

namespace dupr { namespace ast { namespace node {

	class EXCLAM : public dupr<EXCLAM>
	{
	private:
	public:
		EXCLAM(deamer::external::cpp::ast::NodeInformation information_)
		: dupr<EXCLAM>(information_, {}, {})
		{
		}
	};

}}}

#endif // dupr_AST_NODE_EXCLAM_H
