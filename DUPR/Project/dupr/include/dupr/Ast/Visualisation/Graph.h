#ifndef DUPR_AST_VISUALISATION_GRAPH_H
#define DUPR_AST_VISUALISATION_GRAPH_H

#include "dupr/Ast/Listener/EnterExitListener.h"

namespace dupr { namespace ast { namespace listener { namespace deamer { namespace visualisation {

	class Graph : public EnterExitListener
	{
	private:
		std::string output;

		void Init()
		{
			output += "digraph dupr_AST {\n";
		}

		void End()
		{
			output += "}\n";
		}

		void AddConnection(const  ::deamer::external::cpp::ast::Node* source, const  ::deamer::external::cpp::ast::Node* target)
		{
			output += "\t" + std::to_string(::std::size_t(source)) + " -> " + std::to_string(::std::size_t(target)) + ";\n";
		}

		
		void ListenEntry(const ::dupr::ast::node::program* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"program\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::deamerreserved_star__stmt__* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"deamerreserved_star__stmt__\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::stmt* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"stmt\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::pattern_execution* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"pattern_execution\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::pattern_constructor_array* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"pattern_constructor_array\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::pattern_constructor* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"pattern_constructor\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::pattern_type* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"pattern_type\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::pattern_name* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"pattern_name\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::deamerreserved_arrow__VARNAME__* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"deamerreserved_arrow__VARNAME__\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::deamerreserved_star__GT__VARNAME__* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"deamerreserved_star__GT__VARNAME__\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::pattern_constructor_content* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"pattern_constructor_content\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"deamerreserved_star__pattern_constructor_content_stmt__\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::pattern_constructor_content_stmt* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"pattern_constructor_content_stmt\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::pattern_constructor_operator* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"pattern_constructor_operator\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::pattern_constructor_structure* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"pattern_constructor_structure\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::pattern_constructor_terminate* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"pattern_constructor_terminate\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::pattern_constructor_encapsulation* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"pattern_constructor_encapsulation\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::LEFT_BRACKET* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"LEFT_BRACKET\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::RIGHT_BRACKET* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"RIGHT_BRACKET\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::LEFT_PARANTHESIS* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"LEFT_PARANTHESIS\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::RIGHT_PARANTHESIS* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"RIGHT_PARANTHESIS\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::LEFT_SQUARE_BRACKET* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"LEFT_SQUARE_BRACKET\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::RIGHT_SQUARE_BRACKET* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"RIGHT_SQUARE_BRACKET\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::ADD* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"ADD\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::MINUS* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"MINUS\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::MULTI* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"MULTI\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::DIVIDE* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"DIVIDE\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::LT* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"LT\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::LE* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"LE\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::GT* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"GT\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::GE* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"GE\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::EQ* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"EQ\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::EQEQ* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"EQEQ\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::EQEQEQ* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"EQEQEQ\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::OR* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"OR\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::AND* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"AND\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::OROR* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"OROR\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::ANDAND* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"ANDAND\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::WILDCARD_OP* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"WILDCARD_OP\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::DOT* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"DOT\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::COMMA* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"COMMA\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::COLON* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"COLON\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::SEMICOLON* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"SEMICOLON\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::SIGN* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"SIGN\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::HEKJE* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"HEKJE\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::QUESTION* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"QUESTION\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::EXCLAM* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"EXCLAM\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::PATTERN_INSERTION* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"PATTERN_INSERTION\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::VARNAME* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"VARNAME\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::NUMBER* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"NUMBER\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::DECIMAL* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"DECIMAL\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::STRING* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"STRING\"];\n";
		}

		void ListenEntry(const ::dupr::ast::node::ESCAPE_CHARS* node) override
		{
			for (const auto* child : node->GetNodes())
			{
				AddConnection(node, child);
			}

			output += "\t" + std::to_string(::std::size_t(node)) + " [label=\"ESCAPE_CHARS\"];\n";
		}

		
		void ListenExit(const ::dupr::ast::node::program* node) override
		{
		}

		void ListenExit(const ::dupr::ast::node::deamerreserved_star__stmt__* node) override
		{
		}

		void ListenExit(const ::dupr::ast::node::stmt* node) override
		{
		}

		void ListenExit(const ::dupr::ast::node::pattern_execution* node) override
		{
		}

		void ListenExit(const ::dupr::ast::node::pattern_constructor_array* node) override
		{
		}

		void ListenExit(const ::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____* node) override
		{
		}

		void ListenExit(const ::dupr::ast::node::pattern_constructor* node) override
		{
		}

		void ListenExit(const ::dupr::ast::node::pattern_type* node) override
		{
		}

		void ListenExit(const ::dupr::ast::node::pattern_name* node) override
		{
		}

		void ListenExit(const ::dupr::ast::node::deamerreserved_arrow__VARNAME__* node) override
		{
		}

		void ListenExit(const ::dupr::ast::node::deamerreserved_star__GT__VARNAME__* node) override
		{
		}

		void ListenExit(const ::dupr::ast::node::pattern_constructor_content* node) override
		{
		}

		void ListenExit(const ::dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__* node) override
		{
		}

		void ListenExit(const ::dupr::ast::node::pattern_constructor_content_stmt* node) override
		{
		}

		void ListenExit(const ::dupr::ast::node::pattern_constructor_operator* node) override
		{
		}

		void ListenExit(const ::dupr::ast::node::pattern_constructor_structure* node) override
		{
		}

		void ListenExit(const ::dupr::ast::node::pattern_constructor_terminate* node) override
		{
		}

		void ListenExit(const ::dupr::ast::node::pattern_constructor_encapsulation* node) override
		{
		}


	public:
		Graph()
		{
			Init();
		}


		std::string GetGraph()
		{
			End();
			return output;
		}
	};

}}}}}

#endif // DUPR_AST_VISUALISATION_GRAPH_H