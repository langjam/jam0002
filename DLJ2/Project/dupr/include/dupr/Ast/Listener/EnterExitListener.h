#ifndef DUPR_AST_LISTENER_ENTEREXITLISTENER_H
#define DUPR_AST_LISTENER_ENTEREXITLISTENER_H

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
#include "dupr/Ast/Node/EQEQ.h"
#include "dupr/Ast/Node/EQEQEQ.h"
#include "dupr/Ast/Node/OR.h"
#include "dupr/Ast/Node/AND.h"
#include "dupr/Ast/Node/OROR.h"
#include "dupr/Ast/Node/ANDAND.h"
#include "dupr/Ast/Node/WILDCARD_OP.h"
#include "dupr/Ast/Node/DOT.h"
#include "dupr/Ast/Node/COMMA.h"
#include "dupr/Ast/Node/COLON.h"
#include "dupr/Ast/Node/SEMICOLON.h"
#include "dupr/Ast/Node/SIGN.h"
#include "dupr/Ast/Node/HEKJE.h"
#include "dupr/Ast/Node/QUESTION.h"
#include "dupr/Ast/Node/EXCLAM.h"
#include "dupr/Ast/Node/PATTERN_INSERTION.h"
#include "dupr/Ast/Node/VARNAME.h"
#include "dupr/Ast/Node/NUMBER.h"
#include "dupr/Ast/Node/DECIMAL.h"
#include "dupr/Ast/Node/ESCAPE_CHARS.h"


#include "dupr/Ast/Node/program.h"
#include "dupr/Ast/Node/deamerreserved_star__stmt__.h"
#include "dupr/Ast/Node/stmt.h"
#include "dupr/Ast/Node/pattern_execution.h"
#include "dupr/Ast/Node/pattern_constructor_array.h"
#include "dupr/Ast/Node/deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____.h"
#include "dupr/Ast/Node/pattern_constructor.h"
#include "dupr/Ast/Node/pattern_type.h"
#include "dupr/Ast/Node/pattern_name.h"
#include "dupr/Ast/Node/deamerreserved_arrow__VARNAME__.h"
#include "dupr/Ast/Node/deamerreserved_star__GT__VARNAME__.h"
#include "dupr/Ast/Node/pattern_constructor_content.h"
#include "dupr/Ast/Node/deamerreserved_star__pattern_constructor_content_stmt__.h"
#include "dupr/Ast/Node/pattern_constructor_content_stmt.h"
#include "dupr/Ast/Node/pattern_constructor_operator.h"
#include "dupr/Ast/Node/pattern_constructor_structure.h"
#include "dupr/Ast/Node/pattern_constructor_terminate.h"
#include "dupr/Ast/Node/pattern_constructor_encapsulation.h"

#include <Deamer/External/Cpp/Ast/Listener.h>
#include <Deamer/Algorithm/Tree/DFS.h>

namespace dupr { namespace ast { namespace listener { 

	class EnterExitListener : public ::deamer::external::cpp::ast::Listener
	{
	private:
	public:
		EnterExitListener() = default;
		~EnterExitListener() override = default;

	public:
		void Dispatch(const ::deamer::external::cpp::ast::Node* node)  override
		{
			::deamer::algorithm::tree::DFS::Execute::Heap::Search(node,
				&::deamer::external::cpp::ast::Node::GetParent,
				&::deamer::external::cpp::ast::Node::GetNodes,
				&EnterExitListener::DispatchEntry,
				&EnterExitListener::DispatchExit,
				this);
		}

		void DispatchEntry(const ::deamer::external::cpp::ast::Node* node) 
		{
			const auto enumeratedValue = static_cast<dupr::ast::Type>(node->GetType());
			switch(enumeratedValue)
			{
			// Terminal cases
			
			case dupr::ast::Type::LEFT_BRACKET:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::LEFT_BRACKET*>(node));
				break;
			}

			case dupr::ast::Type::RIGHT_BRACKET:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::RIGHT_BRACKET*>(node));
				break;
			}

			case dupr::ast::Type::LEFT_PARANTHESIS:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::LEFT_PARANTHESIS*>(node));
				break;
			}

			case dupr::ast::Type::RIGHT_PARANTHESIS:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::RIGHT_PARANTHESIS*>(node));
				break;
			}

			case dupr::ast::Type::LEFT_SQUARE_BRACKET:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::LEFT_SQUARE_BRACKET*>(node));
				break;
			}

			case dupr::ast::Type::RIGHT_SQUARE_BRACKET:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::RIGHT_SQUARE_BRACKET*>(node));
				break;
			}

			case dupr::ast::Type::ADD:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::ADD*>(node));
				break;
			}

			case dupr::ast::Type::MINUS:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::MINUS*>(node));
				break;
			}

			case dupr::ast::Type::MULTI:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::MULTI*>(node));
				break;
			}

			case dupr::ast::Type::DIVIDE:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::DIVIDE*>(node));
				break;
			}

			case dupr::ast::Type::LT:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::LT*>(node));
				break;
			}

			case dupr::ast::Type::LE:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::LE*>(node));
				break;
			}

			case dupr::ast::Type::GT:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::GT*>(node));
				break;
			}

			case dupr::ast::Type::GE:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::GE*>(node));
				break;
			}

			case dupr::ast::Type::EQ:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::EQ*>(node));
				break;
			}

			case dupr::ast::Type::EQEQ:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::EQEQ*>(node));
				break;
			}

			case dupr::ast::Type::EQEQEQ:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::EQEQEQ*>(node));
				break;
			}

			case dupr::ast::Type::OR:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::OR*>(node));
				break;
			}

			case dupr::ast::Type::AND:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::AND*>(node));
				break;
			}

			case dupr::ast::Type::OROR:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::OROR*>(node));
				break;
			}

			case dupr::ast::Type::ANDAND:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::ANDAND*>(node));
				break;
			}

			case dupr::ast::Type::WILDCARD_OP:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::WILDCARD_OP*>(node));
				break;
			}

			case dupr::ast::Type::DOT:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::DOT*>(node));
				break;
			}

			case dupr::ast::Type::COMMA:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::COMMA*>(node));
				break;
			}

			case dupr::ast::Type::COLON:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::COLON*>(node));
				break;
			}

			case dupr::ast::Type::SEMICOLON:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::SEMICOLON*>(node));
				break;
			}

			case dupr::ast::Type::SIGN:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::SIGN*>(node));
				break;
			}

			case dupr::ast::Type::HEKJE:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::HEKJE*>(node));
				break;
			}

			case dupr::ast::Type::QUESTION:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::QUESTION*>(node));
				break;
			}

			case dupr::ast::Type::EXCLAM:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::EXCLAM*>(node));
				break;
			}

			case dupr::ast::Type::PATTERN_INSERTION:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::PATTERN_INSERTION*>(node));
				break;
			}

			case dupr::ast::Type::VARNAME:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::VARNAME*>(node));
				break;
			}

			case dupr::ast::Type::NUMBER:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::NUMBER*>(node));
				break;
			}

			case dupr::ast::Type::DECIMAL:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::DECIMAL*>(node));
				break;
			}

			case dupr::ast::Type::ESCAPE_CHARS:
			{
				// Entry terminal
				EnterAnything(node);
				EnterTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::ESCAPE_CHARS*>(node));
				break;
			}


			// Nonterminal cases
			
			case dupr::ast::Type::program:
			{
				// Enter nonterminal
				EnterAnything(node);
				EnterNonTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::program*>(node));
				break;
			}

			case dupr::ast::Type::deamerreserved_star__stmt__:
			{
				// Enter nonterminal
				EnterAnything(node);
				EnterNonTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::deamerreserved_star__stmt__*>(node));
				break;
			}

			case dupr::ast::Type::stmt:
			{
				// Enter nonterminal
				EnterAnything(node);
				EnterNonTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::stmt*>(node));
				break;
			}

			case dupr::ast::Type::pattern_execution:
			{
				// Enter nonterminal
				EnterAnything(node);
				EnterNonTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::pattern_execution*>(node));
				break;
			}

			case dupr::ast::Type::pattern_constructor_array:
			{
				// Enter nonterminal
				EnterAnything(node);
				EnterNonTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::pattern_constructor_array*>(node));
				break;
			}

			case dupr::ast::Type::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____:
			{
				// Enter nonterminal
				EnterAnything(node);
				EnterNonTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____*>(node));
				break;
			}

			case dupr::ast::Type::pattern_constructor:
			{
				// Enter nonterminal
				EnterAnything(node);
				EnterNonTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::pattern_constructor*>(node));
				break;
			}

			case dupr::ast::Type::pattern_type:
			{
				// Enter nonterminal
				EnterAnything(node);
				EnterNonTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::pattern_type*>(node));
				break;
			}

			case dupr::ast::Type::pattern_name:
			{
				// Enter nonterminal
				EnterAnything(node);
				EnterNonTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::pattern_name*>(node));
				break;
			}

			case dupr::ast::Type::deamerreserved_arrow__VARNAME__:
			{
				// Enter nonterminal
				EnterAnything(node);
				EnterNonTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::deamerreserved_arrow__VARNAME__*>(node));
				break;
			}

			case dupr::ast::Type::deamerreserved_star__GT__VARNAME__:
			{
				// Enter nonterminal
				EnterAnything(node);
				EnterNonTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::deamerreserved_star__GT__VARNAME__*>(node));
				break;
			}

			case dupr::ast::Type::pattern_constructor_content:
			{
				// Enter nonterminal
				EnterAnything(node);
				EnterNonTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::pattern_constructor_content*>(node));
				break;
			}

			case dupr::ast::Type::deamerreserved_star__pattern_constructor_content_stmt__:
			{
				// Enter nonterminal
				EnterAnything(node);
				EnterNonTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__*>(node));
				break;
			}

			case dupr::ast::Type::pattern_constructor_content_stmt:
			{
				// Enter nonterminal
				EnterAnything(node);
				EnterNonTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::pattern_constructor_content_stmt*>(node));
				break;
			}

			case dupr::ast::Type::pattern_constructor_operator:
			{
				// Enter nonterminal
				EnterAnything(node);
				EnterNonTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::pattern_constructor_operator*>(node));
				break;
			}

			case dupr::ast::Type::pattern_constructor_structure:
			{
				// Enter nonterminal
				EnterAnything(node);
				EnterNonTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::pattern_constructor_structure*>(node));
				break;
			}

			case dupr::ast::Type::pattern_constructor_terminate:
			{
				// Enter nonterminal
				EnterAnything(node);
				EnterNonTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::pattern_constructor_terminate*>(node));
				break;
			}

			case dupr::ast::Type::pattern_constructor_encapsulation:
			{
				// Enter nonterminal
				EnterAnything(node);
				EnterNonTerminal(node);
				ListenEntry(static_cast<const dupr::ast::node::pattern_constructor_encapsulation*>(node));
				break;
			}

			}
		}

		void DispatchExit(const ::deamer::external::cpp::ast::Node* node) 
		{
			const auto enumeratedValue = static_cast<dupr::ast::Type>(node->GetType());
			switch(enumeratedValue)
			{
			// Terminal cases
			
			case dupr::ast::Type::LEFT_BRACKET:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::LEFT_BRACKET*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::RIGHT_BRACKET:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::RIGHT_BRACKET*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::LEFT_PARANTHESIS:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::LEFT_PARANTHESIS*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::RIGHT_PARANTHESIS:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::RIGHT_PARANTHESIS*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::LEFT_SQUARE_BRACKET:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::LEFT_SQUARE_BRACKET*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::RIGHT_SQUARE_BRACKET:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::RIGHT_SQUARE_BRACKET*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::ADD:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::ADD*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::MINUS:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::MINUS*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::MULTI:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::MULTI*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::DIVIDE:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::DIVIDE*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::LT:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::LT*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::LE:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::LE*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::GT:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::GT*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::GE:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::GE*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::EQ:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::EQ*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::EQEQ:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::EQEQ*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::EQEQEQ:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::EQEQEQ*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::OR:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::OR*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::AND:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::AND*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::OROR:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::OROR*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::ANDAND:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::ANDAND*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::WILDCARD_OP:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::WILDCARD_OP*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::DOT:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::DOT*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::COMMA:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::COMMA*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::COLON:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::COLON*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::SEMICOLON:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::SEMICOLON*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::SIGN:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::SIGN*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::HEKJE:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::HEKJE*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::QUESTION:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::QUESTION*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::EXCLAM:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::EXCLAM*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::PATTERN_INSERTION:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::PATTERN_INSERTION*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::VARNAME:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::VARNAME*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::NUMBER:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::NUMBER*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::DECIMAL:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::DECIMAL*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::ESCAPE_CHARS:
			{
				// Exit terminal
				ListenExit(static_cast<const dupr::ast::node::ESCAPE_CHARS*>(node));
				ExitTerminal(node);
				ExitAnything(node);
				break;
			}


			// Nonterminal cases
			
			case dupr::ast::Type::program:
			{
				// Exit nonterminal
				ListenExit(static_cast<const dupr::ast::node::program*>(node));
				ExitNonTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::deamerreserved_star__stmt__:
			{
				// Exit nonterminal
				ListenExit(static_cast<const dupr::ast::node::deamerreserved_star__stmt__*>(node));
				ExitNonTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::stmt:
			{
				// Exit nonterminal
				ListenExit(static_cast<const dupr::ast::node::stmt*>(node));
				ExitNonTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::pattern_execution:
			{
				// Exit nonterminal
				ListenExit(static_cast<const dupr::ast::node::pattern_execution*>(node));
				ExitNonTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::pattern_constructor_array:
			{
				// Exit nonterminal
				ListenExit(static_cast<const dupr::ast::node::pattern_constructor_array*>(node));
				ExitNonTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____:
			{
				// Exit nonterminal
				ListenExit(static_cast<const dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____*>(node));
				ExitNonTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::pattern_constructor:
			{
				// Exit nonterminal
				ListenExit(static_cast<const dupr::ast::node::pattern_constructor*>(node));
				ExitNonTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::pattern_type:
			{
				// Exit nonterminal
				ListenExit(static_cast<const dupr::ast::node::pattern_type*>(node));
				ExitNonTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::pattern_name:
			{
				// Exit nonterminal
				ListenExit(static_cast<const dupr::ast::node::pattern_name*>(node));
				ExitNonTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::deamerreserved_arrow__VARNAME__:
			{
				// Exit nonterminal
				ListenExit(static_cast<const dupr::ast::node::deamerreserved_arrow__VARNAME__*>(node));
				ExitNonTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::deamerreserved_star__GT__VARNAME__:
			{
				// Exit nonterminal
				ListenExit(static_cast<const dupr::ast::node::deamerreserved_star__GT__VARNAME__*>(node));
				ExitNonTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::pattern_constructor_content:
			{
				// Exit nonterminal
				ListenExit(static_cast<const dupr::ast::node::pattern_constructor_content*>(node));
				ExitNonTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::deamerreserved_star__pattern_constructor_content_stmt__:
			{
				// Exit nonterminal
				ListenExit(static_cast<const dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__*>(node));
				ExitNonTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::pattern_constructor_content_stmt:
			{
				// Exit nonterminal
				ListenExit(static_cast<const dupr::ast::node::pattern_constructor_content_stmt*>(node));
				ExitNonTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::pattern_constructor_operator:
			{
				// Exit nonterminal
				ListenExit(static_cast<const dupr::ast::node::pattern_constructor_operator*>(node));
				ExitNonTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::pattern_constructor_structure:
			{
				// Exit nonterminal
				ListenExit(static_cast<const dupr::ast::node::pattern_constructor_structure*>(node));
				ExitNonTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::pattern_constructor_terminate:
			{
				// Exit nonterminal
				ListenExit(static_cast<const dupr::ast::node::pattern_constructor_terminate*>(node));
				ExitNonTerminal(node);
				ExitAnything(node);
				break;
			}

			case dupr::ast::Type::pattern_constructor_encapsulation:
			{
				// Exit nonterminal
				ListenExit(static_cast<const dupr::ast::node::pattern_constructor_encapsulation*>(node));
				ExitNonTerminal(node);
				ExitAnything(node);
				break;
			}

			}
		}

		
		virtual void ListenEntry(const dupr::ast::node::LEFT_BRACKET* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::RIGHT_BRACKET* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::LEFT_PARANTHESIS* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::RIGHT_PARANTHESIS* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::LEFT_SQUARE_BRACKET* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::RIGHT_SQUARE_BRACKET* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::ADD* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::MINUS* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::MULTI* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::DIVIDE* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::LT* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::LE* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::GT* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::GE* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::EQ* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::EQEQ* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::EQEQEQ* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::OR* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::AND* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::OROR* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::ANDAND* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::WILDCARD_OP* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::DOT* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::COMMA* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::COLON* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::SEMICOLON* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::SIGN* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::HEKJE* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::QUESTION* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::EXCLAM* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::PATTERN_INSERTION* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::VARNAME* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::NUMBER* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::DECIMAL* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::ESCAPE_CHARS* node) 
		{
		}

		
		virtual void ListenExit(const dupr::ast::node::LEFT_BRACKET* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::RIGHT_BRACKET* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::LEFT_PARANTHESIS* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::RIGHT_PARANTHESIS* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::LEFT_SQUARE_BRACKET* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::RIGHT_SQUARE_BRACKET* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::ADD* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::MINUS* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::MULTI* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::DIVIDE* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::LT* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::LE* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::GT* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::GE* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::EQ* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::EQEQ* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::EQEQEQ* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::OR* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::AND* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::OROR* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::ANDAND* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::WILDCARD_OP* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::DOT* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::COMMA* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::COLON* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::SEMICOLON* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::SIGN* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::HEKJE* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::QUESTION* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::EXCLAM* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::PATTERN_INSERTION* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::VARNAME* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::NUMBER* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::DECIMAL* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::ESCAPE_CHARS* node) 
		{
		}


		
		virtual void ListenEntry(const dupr::ast::node::program* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::deamerreserved_star__stmt__* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::stmt* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::pattern_execution* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::pattern_constructor_array* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::pattern_constructor* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::pattern_type* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::pattern_name* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::deamerreserved_arrow__VARNAME__* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::deamerreserved_star__GT__VARNAME__* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::pattern_constructor_content* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::pattern_constructor_content_stmt* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::pattern_constructor_operator* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::pattern_constructor_structure* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::pattern_constructor_terminate* node) 
		{
		}

		virtual void ListenEntry(const dupr::ast::node::pattern_constructor_encapsulation* node) 
		{
		}

		
		virtual void ListenExit(const dupr::ast::node::program* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::deamerreserved_star__stmt__* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::stmt* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::pattern_execution* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::pattern_constructor_array* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::pattern_constructor* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::pattern_type* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::pattern_name* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::deamerreserved_arrow__VARNAME__* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::deamerreserved_star__GT__VARNAME__* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::pattern_constructor_content* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::pattern_constructor_content_stmt* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::pattern_constructor_operator* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::pattern_constructor_structure* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::pattern_constructor_terminate* node) 
		{
		}

		virtual void ListenExit(const dupr::ast::node::pattern_constructor_encapsulation* node) 
		{
		}


		
		virtual void EnterTerminal(const ::deamer::external::cpp::ast::Node* node) 
		{
		}
		
		virtual void ExitTerminal(const ::deamer::external::cpp::ast::Node* node) 
		{
		}
		
		virtual void EnterNonTerminal(const ::deamer::external::cpp::ast::Node* node) 
		{
		}
		
		virtual void ExitNonTerminal(const ::deamer::external::cpp::ast::Node* node) 
		{
		}
		
		virtual void EnterAnything(const ::deamer::external::cpp::ast::Node* node) 
		{
		}

		virtual void ExitAnything(const ::deamer::external::cpp::ast::Node* node) 
		{
		}
	};

}}}

#endif // DUPR_AST_LISTENER_ENTEREXITLISTENER_H