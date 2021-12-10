#ifndef dupr_AST_NODE_dupr_H
#define dupr_AST_NODE_dupr_H

#include <Deamer/External/Cpp/Ast/TemplateNodeBase.h>

namespace dupr { namespace ast { namespace node {

	template<typename Derived>
	class dupr : public ::deamer::external::cpp::ast::TemplateNodeBase<dupr<Derived>, Derived>
	{
	private:
		
	public:
		dupr() = default;
		
		dupr(deamer::external::cpp::ast::NodeInformation information_, std::vector<deamer::external::cpp::ast::Node*> nodes_ = {}, std::vector<size_t> baseValues_ = {})
		: deamer::external::cpp::ast::TemplateNodeBase<dupr<Derived>, Derived>(information_, nodes_, baseValues_)
		{
		}
	};

}}}

#endif // dupr_AST_NODE_dupr_H
