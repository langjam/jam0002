#ifndef dupr_AST_LISTENER_DEAMER_LISTENER_H
#define dupr_AST_LISTENER_DEAMER_LISTENER_H

#include <Deamer/External/Cpp/Ast/Listener.h>
#include "dupr/Ast/Node/dupr.h"
#include "dupr/Ast/Enum/Type.h"
#include "dupr/Ast/Node/LEFT_BRACKET.h"
#include "dupr/Ast/Node/RIGHT_BRACKET.h"
#include "dupr/Ast/Node/LEFT_PARANTHESIS.h"
#include "dupr/Ast/Node/RIGHT_PARANTHESIS.h"
#include "dupr/Ast/Node/LEFT_SQUARE_BRACKET.h"
#include "dupr/Ast/Node/RIGHT_SQUARE_BRACKET.h"
#include "dupr/Ast/Node/ADD.h"
#include "dupr/Ast/Node/MINUS.h"
#include "dupr/Ast/Node/MULTI.h"
#include "dupr/Ast/Node/DIVIDE.h"
#include "dupr/Ast/Node/LT.h"
#include "dupr/Ast/Node/LE.h"
#include "dupr/Ast/Node/GT.h"
#include "dupr/Ast/Node/GE.h"
#include "dupr/Ast/Node/EQ.h"
#include "dupr/Ast/Node/DOT.h"
#include "dupr/Ast/Node/COMMA.h"
#include "dupr/Ast/Node/COLON.h"
#include "dupr/Ast/Node/SEMICOLON.h"
#include "dupr/Ast/Node/PATTERN_INSERTION.h"
#include "dupr/Ast/Node/VARNAME.h"
#include "dupr/Ast/Node/NUMBER.h"
#include "dupr/Ast/Node/DECIMAL.h"
#include "dupr/Ast/Node/ESCAPE_CHARS.h"

#include "dupr/Ast/Node/program.h"
#include "dupr/Ast/Node/deamerreserved_star__stmt__.h"
#include "dupr/Ast/Node/stmt.h"
#include "dupr/Ast/Node/pattern_execution.h"
#include "dupr/Ast/Node/pattern_execution_content.h"
#include "dupr/Ast/Node/deamerreserved_plus__pattern_execution_content_stmt__.h"
#include "dupr/Ast/Node/pattern_execution_content_stmt.h"
#include "dupr/Ast/Node/pattern_constructor.h"
#include "dupr/Ast/Node/pattern_type.h"
#include "dupr/Ast/Node/pattern_name.h"
#include "dupr/Ast/Node/pattern_constructor_content.h"
#include "dupr/Ast/Node/deamerreserved_plus__pattern_constructor_content_stmt__.h"
#include "dupr/Ast/Node/pattern_constructor_content_stmt.h"
#include "dupr/Ast/Node/pattern_constructor_operator.h"
#include "dupr/Ast/Node/pattern_constructor_structure.h"
#include "dupr/Ast/Node/pattern_constructor_terminate.h"
#include "dupr/Ast/Node/pattern_constructor_encapsulation.h"

namespace dupr { namespace ast { namespace listener { 

	class Listener : public ::deamer::external::cpp::ast::Listener
	{
	private:
	public:
		Listener() = default;
		~Listener() override = default;
	public:
		void Dispatch(const ::deamer::external::cpp::ast::Node* node) override
		{
			const auto enumeratedValue = static_cast<dupr::ast::Type>(node->GetType());
			switch(enumeratedValue)
			{
			case dupr::ast::Type::LEFT_BRACKET:
			{
				Listen(static_cast<const dupr::ast::node::LEFT_BRACKET*>(node));
				break;
			}
			case dupr::ast::Type::RIGHT_BRACKET:
			{
				Listen(static_cast<const dupr::ast::node::RIGHT_BRACKET*>(node));
				break;
			}
			case dupr::ast::Type::LEFT_PARANTHESIS:
			{
				Listen(static_cast<const dupr::ast::node::LEFT_PARANTHESIS*>(node));
				break;
			}
			case dupr::ast::Type::RIGHT_PARANTHESIS:
			{
				Listen(static_cast<const dupr::ast::node::RIGHT_PARANTHESIS*>(node));
				break;
			}
			case dupr::ast::Type::LEFT_SQUARE_BRACKET:
			{
				Listen(static_cast<const dupr::ast::node::LEFT_SQUARE_BRACKET*>(node));
				break;
			}
			case dupr::ast::Type::RIGHT_SQUARE_BRACKET:
			{
				Listen(static_cast<const dupr::ast::node::RIGHT_SQUARE_BRACKET*>(node));
				break;
			}
			case dupr::ast::Type::ADD:
			{
				Listen(static_cast<const dupr::ast::node::ADD*>(node));
				break;
			}
			case dupr::ast::Type::MINUS:
			{
				Listen(static_cast<const dupr::ast::node::MINUS*>(node));
				break;
			}
			case dupr::ast::Type::MULTI:
			{
				Listen(static_cast<const dupr::ast::node::MULTI*>(node));
				break;
			}
			case dupr::ast::Type::DIVIDE:
			{
				Listen(static_cast<const dupr::ast::node::DIVIDE*>(node));
				break;
			}
			case dupr::ast::Type::LT:
			{
				Listen(static_cast<const dupr::ast::node::LT*>(node));
				break;
			}
			case dupr::ast::Type::LE:
			{
				Listen(static_cast<const dupr::ast::node::LE*>(node));
				break;
			}
			case dupr::ast::Type::GT:
			{
				Listen(static_cast<const dupr::ast::node::GT*>(node));
				break;
			}
			case dupr::ast::Type::GE:
			{
				Listen(static_cast<const dupr::ast::node::GE*>(node));
				break;
			}
			case dupr::ast::Type::EQ:
			{
				Listen(static_cast<const dupr::ast::node::EQ*>(node));
				break;
			}
			case dupr::ast::Type::DOT:
			{
				Listen(static_cast<const dupr::ast::node::DOT*>(node));
				break;
			}
			case dupr::ast::Type::COMMA:
			{
				Listen(static_cast<const dupr::ast::node::COMMA*>(node));
				break;
			}
			case dupr::ast::Type::COLON:
			{
				Listen(static_cast<const dupr::ast::node::COLON*>(node));
				break;
			}
			case dupr::ast::Type::SEMICOLON:
			{
				Listen(static_cast<const dupr::ast::node::SEMICOLON*>(node));
				break;
			}
			case dupr::ast::Type::PATTERN_INSERTION:
			{
				Listen(static_cast<const dupr::ast::node::PATTERN_INSERTION*>(node));
				break;
			}
			case dupr::ast::Type::VARNAME:
			{
				Listen(static_cast<const dupr::ast::node::VARNAME*>(node));
				break;
			}
			case dupr::ast::Type::NUMBER:
			{
				Listen(static_cast<const dupr::ast::node::NUMBER*>(node));
				break;
			}
			case dupr::ast::Type::DECIMAL:
			{
				Listen(static_cast<const dupr::ast::node::DECIMAL*>(node));
				break;
			}
			case dupr::ast::Type::ESCAPE_CHARS:
			{
				Listen(static_cast<const dupr::ast::node::ESCAPE_CHARS*>(node));
				break;
			}

			case dupr::ast::Type::program:
			{
				Listen(static_cast<const dupr::ast::node::program*>(node));
				DefaultAction(node);
				break;
			}
			case dupr::ast::Type::deamerreserved_star__stmt__:
			{
				Listen(static_cast<const dupr::ast::node::deamerreserved_star__stmt__*>(node));
				DefaultAction(node);
				break;
			}
			case dupr::ast::Type::stmt:
			{
				Listen(static_cast<const dupr::ast::node::stmt*>(node));
				DefaultAction(node);
				break;
			}
			case dupr::ast::Type::pattern_execution:
			{
				Listen(static_cast<const dupr::ast::node::pattern_execution*>(node));
				DefaultAction(node);
				break;
			}
			case dupr::ast::Type::pattern_execution_content:
			{
				Listen(static_cast<const dupr::ast::node::pattern_execution_content*>(node));
				DefaultAction(node);
				break;
			}
			case dupr::ast::Type::deamerreserved_plus__pattern_execution_content_stmt__:
			{
				Listen(static_cast<const dupr::ast::node::deamerreserved_plus__pattern_execution_content_stmt__*>(node));
				DefaultAction(node);
				break;
			}
			case dupr::ast::Type::pattern_execution_content_stmt:
			{
				Listen(static_cast<const dupr::ast::node::pattern_execution_content_stmt*>(node));
				DefaultAction(node);
				break;
			}
			case dupr::ast::Type::pattern_constructor:
			{
				Listen(static_cast<const dupr::ast::node::pattern_constructor*>(node));
				DefaultAction(node);
				break;
			}
			case dupr::ast::Type::pattern_type:
			{
				Listen(static_cast<const dupr::ast::node::pattern_type*>(node));
				DefaultAction(node);
				break;
			}
			case dupr::ast::Type::pattern_name:
			{
				Listen(static_cast<const dupr::ast::node::pattern_name*>(node));
				DefaultAction(node);
				break;
			}
			case dupr::ast::Type::pattern_constructor_content:
			{
				Listen(static_cast<const dupr::ast::node::pattern_constructor_content*>(node));
				DefaultAction(node);
				break;
			}
			case dupr::ast::Type::deamerreserved_plus__pattern_constructor_content_stmt__:
			{
				Listen(static_cast<const dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__*>(node));
				DefaultAction(node);
				break;
			}
			case dupr::ast::Type::pattern_constructor_content_stmt:
			{
				Listen(static_cast<const dupr::ast::node::pattern_constructor_content_stmt*>(node));
				DefaultAction(node);
				break;
			}
			case dupr::ast::Type::pattern_constructor_operator:
			{
				Listen(static_cast<const dupr::ast::node::pattern_constructor_operator*>(node));
				DefaultAction(node);
				break;
			}
			case dupr::ast::Type::pattern_constructor_structure:
			{
				Listen(static_cast<const dupr::ast::node::pattern_constructor_structure*>(node));
				DefaultAction(node);
				break;
			}
			case dupr::ast::Type::pattern_constructor_terminate:
			{
				Listen(static_cast<const dupr::ast::node::pattern_constructor_terminate*>(node));
				DefaultAction(node);
				break;
			}
			case dupr::ast::Type::pattern_constructor_encapsulation:
			{
				Listen(static_cast<const dupr::ast::node::pattern_constructor_encapsulation*>(node));
				DefaultAction(node);
				break;
			}
			}
		}
		virtual void Listen(const dupr::ast::node::LEFT_BRACKET* node)
		{
		}
		virtual void Listen(const dupr::ast::node::RIGHT_BRACKET* node)
		{
		}
		virtual void Listen(const dupr::ast::node::LEFT_PARANTHESIS* node)
		{
		}
		virtual void Listen(const dupr::ast::node::RIGHT_PARANTHESIS* node)
		{
		}
		virtual void Listen(const dupr::ast::node::LEFT_SQUARE_BRACKET* node)
		{
		}
		virtual void Listen(const dupr::ast::node::RIGHT_SQUARE_BRACKET* node)
		{
		}
		virtual void Listen(const dupr::ast::node::ADD* node)
		{
		}
		virtual void Listen(const dupr::ast::node::MINUS* node)
		{
		}
		virtual void Listen(const dupr::ast::node::MULTI* node)
		{
		}
		virtual void Listen(const dupr::ast::node::DIVIDE* node)
		{
		}
		virtual void Listen(const dupr::ast::node::LT* node)
		{
		}
		virtual void Listen(const dupr::ast::node::LE* node)
		{
		}
		virtual void Listen(const dupr::ast::node::GT* node)
		{
		}
		virtual void Listen(const dupr::ast::node::GE* node)
		{
		}
		virtual void Listen(const dupr::ast::node::EQ* node)
		{
		}
		virtual void Listen(const dupr::ast::node::DOT* node)
		{
		}
		virtual void Listen(const dupr::ast::node::COMMA* node)
		{
		}
		virtual void Listen(const dupr::ast::node::COLON* node)
		{
		}
		virtual void Listen(const dupr::ast::node::SEMICOLON* node)
		{
		}
		virtual void Listen(const dupr::ast::node::PATTERN_INSERTION* node)
		{
		}
		virtual void Listen(const dupr::ast::node::VARNAME* node)
		{
		}
		virtual void Listen(const dupr::ast::node::NUMBER* node)
		{
		}
		virtual void Listen(const dupr::ast::node::DECIMAL* node)
		{
		}
		virtual void Listen(const dupr::ast::node::ESCAPE_CHARS* node)
		{
		}

		virtual void Listen(const dupr::ast::node::program* node)
		{
		}
		virtual void Listen(const dupr::ast::node::deamerreserved_star__stmt__* node)
		{
		}
		virtual void Listen(const dupr::ast::node::stmt* node)
		{
		}
		virtual void Listen(const dupr::ast::node::pattern_execution* node)
		{
		}
		virtual void Listen(const dupr::ast::node::pattern_execution_content* node)
		{
		}
		virtual void Listen(const dupr::ast::node::deamerreserved_plus__pattern_execution_content_stmt__* node)
		{
		}
		virtual void Listen(const dupr::ast::node::pattern_execution_content_stmt* node)
		{
		}
		virtual void Listen(const dupr::ast::node::pattern_constructor* node)
		{
		}
		virtual void Listen(const dupr::ast::node::pattern_type* node)
		{
		}
		virtual void Listen(const dupr::ast::node::pattern_name* node)
		{
		}
		virtual void Listen(const dupr::ast::node::pattern_constructor_content* node)
		{
		}
		virtual void Listen(const dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__* node)
		{
		}
		virtual void Listen(const dupr::ast::node::pattern_constructor_content_stmt* node)
		{
		}
		virtual void Listen(const dupr::ast::node::pattern_constructor_operator* node)
		{
		}
		virtual void Listen(const dupr::ast::node::pattern_constructor_structure* node)
		{
		}
		virtual void Listen(const dupr::ast::node::pattern_constructor_terminate* node)
		{
		}
		virtual void Listen(const dupr::ast::node::pattern_constructor_encapsulation* node)
		{
		}
	private:
		void DefaultAction(const ::deamer::external::cpp::ast::Node* node)
		{
			for(const auto* child : node->GetNodes())
			{
				Dispatch(child);
			}
		}
	};

}}}

#endif // dupr_AST_LISTENER_DEAMER_LISTENER_H
