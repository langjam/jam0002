#ifndef DUPR_AST_REFERENCE_ACCESS_H
#define DUPR_AST_REFERENCE_ACCESS_H

#include "dupr/Ast/Relation/NodeEnumToType.h"
#include "dupr/Ast/Relation/NodeTypeToEnum.h"
#include "dupr/Ast/Relation/NodeIsInlined.h"

#include "dupr/Ast/Enum/Type.h"
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


#include <vector>
#include <utility>

namespace dupr { namespace ast { namespace reference { 

	struct AccessBase
	{	
	public:
		AccessBase() = default;

	protected:
		template<::dupr::ast::Type T>
		void Get(std::vector<const ::dupr::ast::relation::NodeEnumToType_t<T>*>& foundNodes, const ::deamer::external::cpp::ast::Node* currentNode)
		{
			for(const auto* const node : currentNode->GetNodes())
			{
				if (node->GetType() == static_cast<::std::size_t>(T))
				{
					foundNodes.push_back(static_cast<const  ::dupr::ast::relation::NodeEnumToType_t<T>*>(node));
				}
				else if (::dupr::ast::relation::NodeIsInlined(static_cast<::dupr::ast::Type>(node->GetType())))
				{
					Get<T>(foundNodes, node);
				}
			}
		}

		template<::dupr::ast::Type T, typename Q>
		inline std::vector<const ::dupr::ast::relation::NodeEnumToType_t<T>*> Get(std::vector<const Q*>& currentNodes)
		{
			std::vector<const ::dupr::ast::relation::NodeEnumToType_t<T>*> foundNodes;

			for (const auto* const currentNode : currentNodes)
			{
				AccessBase::Get<T>(foundNodes, currentNode);
			}

			return foundNodes;
		}
	};

	/*!	\class Access
	 *
	 *	\brief Used to access AST nodes. It contains various helper functions to ease navigation through AST nodes.
	 */
	template<typename T>
	struct Access : public AccessBase
	{
		Access() = delete;
		~Access() = delete;
	};

	template<>
	struct Access<::dupr::ast::node::program>;
	template<>
	struct Access<::dupr::ast::node::deamerreserved_star__stmt__>;
	template<>
	struct Access<::dupr::ast::node::stmt>;
	template<>
	struct Access<::dupr::ast::node::pattern_execution>;
	template<>
	struct Access<::dupr::ast::node::pattern_execution_content>;
	template<>
	struct Access<::dupr::ast::node::deamerreserved_plus__pattern_execution_content_stmt__>;
	template<>
	struct Access<::dupr::ast::node::pattern_execution_content_stmt>;
	template<>
	struct Access<::dupr::ast::node::pattern_constructor>;
	template<>
	struct Access<::dupr::ast::node::pattern_type>;
	template<>
	struct Access<::dupr::ast::node::pattern_name>;
	template<>
	struct Access<::dupr::ast::node::pattern_constructor_content>;
	template<>
	struct Access<::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__>;
	template<>
	struct Access<::dupr::ast::node::pattern_constructor_content_stmt>;
	template<>
	struct Access<::dupr::ast::node::pattern_constructor_operator>;
	template<>
	struct Access<::dupr::ast::node::pattern_constructor_structure>;
	template<>
	struct Access<::dupr::ast::node::pattern_constructor_terminate>;
	template<>
	struct Access<::dupr::ast::node::pattern_constructor_encapsulation>;
	template<>
	struct Access<::dupr::ast::node::LEFT_BRACKET>;
	template<>
	struct Access<::dupr::ast::node::RIGHT_BRACKET>;
	template<>
	struct Access<::dupr::ast::node::LEFT_PARANTHESIS>;
	template<>
	struct Access<::dupr::ast::node::RIGHT_PARANTHESIS>;
	template<>
	struct Access<::dupr::ast::node::LEFT_SQUARE_BRACKET>;
	template<>
	struct Access<::dupr::ast::node::RIGHT_SQUARE_BRACKET>;
	template<>
	struct Access<::dupr::ast::node::ADD>;
	template<>
	struct Access<::dupr::ast::node::MINUS>;
	template<>
	struct Access<::dupr::ast::node::MULTI>;
	template<>
	struct Access<::dupr::ast::node::DIVIDE>;
	template<>
	struct Access<::dupr::ast::node::LT>;
	template<>
	struct Access<::dupr::ast::node::LE>;
	template<>
	struct Access<::dupr::ast::node::GT>;
	template<>
	struct Access<::dupr::ast::node::GE>;
	template<>
	struct Access<::dupr::ast::node::EQ>;
	template<>
	struct Access<::dupr::ast::node::DOT>;
	template<>
	struct Access<::dupr::ast::node::COMMA>;
	template<>
	struct Access<::dupr::ast::node::COLON>;
	template<>
	struct Access<::dupr::ast::node::SEMICOLON>;
	template<>
	struct Access<::dupr::ast::node::PATTERN_INSERTION>;
	template<>
	struct Access<::dupr::ast::node::VARNAME>;
	template<>
	struct Access<::dupr::ast::node::NUMBER>;
	template<>
	struct Access<::dupr::ast::node::DECIMAL>;
	template<>
	struct Access<::dupr::ast::node::ESCAPE_CHARS>;


	
	template<>
	struct Access<::dupr::ast::node::program> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::program*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::program*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::program& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::program* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::program>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::program>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::program*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::program*> GetContent()
		{
			return ts;
		}

	public:
		Access<::dupr::ast::node::deamerreserved_star__stmt__> deamerreserved_star__stmt__();
Access<::dupr::ast::node::stmt> stmt();


		template<typename FunctionType>
		Access<::dupr::ast::node::program>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::deamerreserved_star__stmt__> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::deamerreserved_star__stmt__*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::deamerreserved_star__stmt__*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::deamerreserved_star__stmt__& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::deamerreserved_star__stmt__* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::deamerreserved_star__stmt__>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::deamerreserved_star__stmt__>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::deamerreserved_star__stmt__*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::deamerreserved_star__stmt__*> GetContent()
		{
			return ts;
		}

	public:
		Access<::dupr::ast::node::deamerreserved_star__stmt__> deamerreserved_star__stmt__();
Access<::dupr::ast::node::stmt> stmt();


		template<typename FunctionType>
		Access<::dupr::ast::node::deamerreserved_star__stmt__>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::stmt> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::stmt*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::stmt*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::stmt& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::stmt* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::stmt>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::stmt>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::stmt*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::stmt*> GetContent()
		{
			return ts;
		}

	public:
		Access<::dupr::ast::node::pattern_execution> pattern_execution();
Access<::dupr::ast::node::pattern_constructor> pattern_constructor();


		template<typename FunctionType>
		Access<::dupr::ast::node::stmt>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::pattern_execution> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::pattern_execution*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::pattern_execution*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::pattern_execution& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::pattern_execution* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::pattern_execution>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::pattern_execution>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::pattern_execution*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::pattern_execution*> GetContent()
		{
			return ts;
		}

	public:
		Access<::dupr::ast::node::pattern_execution_content> pattern_execution_content();
Access<::dupr::ast::node::pattern_name> pattern_name();
Access<::dupr::ast::node::LEFT_BRACKET> LEFT_BRACKET();
Access<::dupr::ast::node::RIGHT_BRACKET> RIGHT_BRACKET();


		template<typename FunctionType>
		Access<::dupr::ast::node::pattern_execution>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::pattern_execution_content> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::pattern_execution_content*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::pattern_execution_content*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::pattern_execution_content& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::pattern_execution_content* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::pattern_execution_content>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::pattern_execution_content>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::pattern_execution_content*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::pattern_execution_content*> GetContent()
		{
			return ts;
		}

	public:
		Access<::dupr::ast::node::deamerreserved_plus__pattern_execution_content_stmt__> deamerreserved_plus__pattern_execution_content_stmt__();
Access<::dupr::ast::node::pattern_execution_content_stmt> pattern_execution_content_stmt();


		template<typename FunctionType>
		Access<::dupr::ast::node::pattern_execution_content>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::deamerreserved_plus__pattern_execution_content_stmt__> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::deamerreserved_plus__pattern_execution_content_stmt__*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::deamerreserved_plus__pattern_execution_content_stmt__*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::deamerreserved_plus__pattern_execution_content_stmt__& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::deamerreserved_plus__pattern_execution_content_stmt__* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::deamerreserved_plus__pattern_execution_content_stmt__>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::deamerreserved_plus__pattern_execution_content_stmt__>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::deamerreserved_plus__pattern_execution_content_stmt__*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::deamerreserved_plus__pattern_execution_content_stmt__*> GetContent()
		{
			return ts;
		}

	public:
		Access<::dupr::ast::node::deamerreserved_plus__pattern_execution_content_stmt__> deamerreserved_plus__pattern_execution_content_stmt__();
Access<::dupr::ast::node::pattern_execution_content_stmt> pattern_execution_content_stmt();


		template<typename FunctionType>
		Access<::dupr::ast::node::deamerreserved_plus__pattern_execution_content_stmt__>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::pattern_execution_content_stmt> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::pattern_execution_content_stmt*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::pattern_execution_content_stmt*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::pattern_execution_content_stmt& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::pattern_execution_content_stmt* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::pattern_execution_content_stmt>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::pattern_execution_content_stmt>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::pattern_execution_content_stmt*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::pattern_execution_content_stmt*> GetContent()
		{
			return ts;
		}

	public:
		Access<::dupr::ast::node::pattern_constructor_content_stmt> pattern_constructor_content_stmt();


		template<typename FunctionType>
		Access<::dupr::ast::node::pattern_execution_content_stmt>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::pattern_constructor> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::pattern_constructor*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::pattern_constructor*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::pattern_constructor& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::pattern_constructor* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::pattern_constructor>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::pattern_constructor>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::pattern_constructor*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::pattern_constructor*> GetContent()
		{
			return ts;
		}

	public:
		Access<::dupr::ast::node::pattern_type> pattern_type();
Access<::dupr::ast::node::pattern_name> pattern_name();
Access<::dupr::ast::node::pattern_constructor_content> pattern_constructor_content();
Access<::dupr::ast::node::LEFT_BRACKET> LEFT_BRACKET();
Access<::dupr::ast::node::RIGHT_BRACKET> RIGHT_BRACKET();
Access<::dupr::ast::node::COLON> COLON();


		template<typename FunctionType>
		Access<::dupr::ast::node::pattern_constructor>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::pattern_type> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::pattern_type*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::pattern_type*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::pattern_type& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::pattern_type* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::pattern_type>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::pattern_type>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::pattern_type*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::pattern_type*> GetContent()
		{
			return ts;
		}

	public:
		Access<::dupr::ast::node::VARNAME> VARNAME();


		template<typename FunctionType>
		Access<::dupr::ast::node::pattern_type>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::pattern_name> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::pattern_name*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::pattern_name*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::pattern_name& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::pattern_name* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::pattern_name>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::pattern_name>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::pattern_name*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::pattern_name*> GetContent()
		{
			return ts;
		}

	public:
		Access<::dupr::ast::node::VARNAME> VARNAME();


		template<typename FunctionType>
		Access<::dupr::ast::node::pattern_name>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::pattern_constructor_content> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::pattern_constructor_content*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::pattern_constructor_content*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::pattern_constructor_content& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::pattern_constructor_content* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::pattern_constructor_content>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::pattern_constructor_content>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::pattern_constructor_content*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::pattern_constructor_content*> GetContent()
		{
			return ts;
		}

	public:
		Access<::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__> deamerreserved_plus__pattern_constructor_content_stmt__();
Access<::dupr::ast::node::pattern_constructor_content_stmt> pattern_constructor_content_stmt();


		template<typename FunctionType>
		Access<::dupr::ast::node::pattern_constructor_content>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__*> GetContent()
		{
			return ts;
		}

	public:
		Access<::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__> deamerreserved_plus__pattern_constructor_content_stmt__();
Access<::dupr::ast::node::pattern_constructor_content_stmt> pattern_constructor_content_stmt();


		template<typename FunctionType>
		Access<::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::pattern_constructor_content_stmt> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::pattern_constructor_content_stmt*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::pattern_constructor_content_stmt*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::pattern_constructor_content_stmt& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::pattern_constructor_content_stmt* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::pattern_constructor_content_stmt>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::pattern_constructor_content_stmt>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::pattern_constructor_content_stmt*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::pattern_constructor_content_stmt*> GetContent()
		{
			return ts;
		}

	public:
		Access<::dupr::ast::node::pattern_constructor_operator> pattern_constructor_operator();
Access<::dupr::ast::node::pattern_constructor_structure> pattern_constructor_structure();
Access<::dupr::ast::node::pattern_constructor_terminate> pattern_constructor_terminate();
Access<::dupr::ast::node::pattern_constructor_encapsulation> pattern_constructor_encapsulation();


		template<typename FunctionType>
		Access<::dupr::ast::node::pattern_constructor_content_stmt>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::pattern_constructor_operator> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::pattern_constructor_operator*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::pattern_constructor_operator*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::pattern_constructor_operator& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::pattern_constructor_operator* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::pattern_constructor_operator>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::pattern_constructor_operator>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::pattern_constructor_operator*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::pattern_constructor_operator*> GetContent()
		{
			return ts;
		}

	public:
		Access<::dupr::ast::node::ADD> ADD();
Access<::dupr::ast::node::MINUS> MINUS();
Access<::dupr::ast::node::MULTI> MULTI();
Access<::dupr::ast::node::DIVIDE> DIVIDE();
Access<::dupr::ast::node::LT> LT();
Access<::dupr::ast::node::LE> LE();
Access<::dupr::ast::node::GT> GT();
Access<::dupr::ast::node::GE> GE();
Access<::dupr::ast::node::EQ> EQ();


		template<typename FunctionType>
		Access<::dupr::ast::node::pattern_constructor_operator>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::pattern_constructor_structure> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::pattern_constructor_structure*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::pattern_constructor_structure*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::pattern_constructor_structure& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::pattern_constructor_structure* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::pattern_constructor_structure>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::pattern_constructor_structure>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::pattern_constructor_structure*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::pattern_constructor_structure*> GetContent()
		{
			return ts;
		}

	public:
		Access<::dupr::ast::node::DOT> DOT();
Access<::dupr::ast::node::COMMA> COMMA();
Access<::dupr::ast::node::COLON> COLON();
Access<::dupr::ast::node::SEMICOLON> SEMICOLON();


		template<typename FunctionType>
		Access<::dupr::ast::node::pattern_constructor_structure>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::pattern_constructor_terminate> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::pattern_constructor_terminate*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::pattern_constructor_terminate*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::pattern_constructor_terminate& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::pattern_constructor_terminate* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::pattern_constructor_terminate>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::pattern_constructor_terminate>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::pattern_constructor_terminate*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::pattern_constructor_terminate*> GetContent()
		{
			return ts;
		}

	public:
		Access<::dupr::ast::node::PATTERN_INSERTION> PATTERN_INSERTION();
Access<::dupr::ast::node::VARNAME> VARNAME();
Access<::dupr::ast::node::NUMBER> NUMBER();
Access<::dupr::ast::node::DECIMAL> DECIMAL();


		template<typename FunctionType>
		Access<::dupr::ast::node::pattern_constructor_terminate>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::pattern_constructor_encapsulation> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::pattern_constructor_encapsulation*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::pattern_constructor_encapsulation*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::pattern_constructor_encapsulation& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::pattern_constructor_encapsulation* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::pattern_constructor_encapsulation>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::pattern_constructor_encapsulation>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::pattern_constructor_encapsulation*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::pattern_constructor_encapsulation*> GetContent()
		{
			return ts;
		}

	public:
		Access<::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__> deamerreserved_plus__pattern_constructor_content_stmt__();
Access<::dupr::ast::node::pattern_constructor_content_stmt> pattern_constructor_content_stmt();
Access<::dupr::ast::node::LEFT_BRACKET> LEFT_BRACKET();
Access<::dupr::ast::node::RIGHT_BRACKET> RIGHT_BRACKET();
Access<::dupr::ast::node::LEFT_PARANTHESIS> LEFT_PARANTHESIS();
Access<::dupr::ast::node::RIGHT_PARANTHESIS> RIGHT_PARANTHESIS();
Access<::dupr::ast::node::LEFT_SQUARE_BRACKET> LEFT_SQUARE_BRACKET();
Access<::dupr::ast::node::RIGHT_SQUARE_BRACKET> RIGHT_SQUARE_BRACKET();


		template<typename FunctionType>
		Access<::dupr::ast::node::pattern_constructor_encapsulation>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::LEFT_BRACKET> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::LEFT_BRACKET*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::LEFT_BRACKET*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::LEFT_BRACKET& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::LEFT_BRACKET* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::LEFT_BRACKET>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::LEFT_BRACKET>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::LEFT_BRACKET*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::LEFT_BRACKET*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::LEFT_BRACKET>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::RIGHT_BRACKET> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::RIGHT_BRACKET*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::RIGHT_BRACKET*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::RIGHT_BRACKET& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::RIGHT_BRACKET* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::RIGHT_BRACKET>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::RIGHT_BRACKET>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::RIGHT_BRACKET*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::RIGHT_BRACKET*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::RIGHT_BRACKET>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::LEFT_PARANTHESIS> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::LEFT_PARANTHESIS*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::LEFT_PARANTHESIS*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::LEFT_PARANTHESIS& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::LEFT_PARANTHESIS* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::LEFT_PARANTHESIS>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::LEFT_PARANTHESIS>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::LEFT_PARANTHESIS*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::LEFT_PARANTHESIS*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::LEFT_PARANTHESIS>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::RIGHT_PARANTHESIS> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::RIGHT_PARANTHESIS*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::RIGHT_PARANTHESIS*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::RIGHT_PARANTHESIS& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::RIGHT_PARANTHESIS* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::RIGHT_PARANTHESIS>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::RIGHT_PARANTHESIS>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::RIGHT_PARANTHESIS*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::RIGHT_PARANTHESIS*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::RIGHT_PARANTHESIS>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::LEFT_SQUARE_BRACKET> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::LEFT_SQUARE_BRACKET*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::LEFT_SQUARE_BRACKET*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::LEFT_SQUARE_BRACKET& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::LEFT_SQUARE_BRACKET* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::LEFT_SQUARE_BRACKET>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::LEFT_SQUARE_BRACKET>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::LEFT_SQUARE_BRACKET*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::LEFT_SQUARE_BRACKET*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::LEFT_SQUARE_BRACKET>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::RIGHT_SQUARE_BRACKET> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::RIGHT_SQUARE_BRACKET*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::RIGHT_SQUARE_BRACKET*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::RIGHT_SQUARE_BRACKET& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::RIGHT_SQUARE_BRACKET* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::RIGHT_SQUARE_BRACKET>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::RIGHT_SQUARE_BRACKET>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::RIGHT_SQUARE_BRACKET*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::RIGHT_SQUARE_BRACKET*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::RIGHT_SQUARE_BRACKET>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::ADD> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::ADD*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::ADD*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::ADD& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::ADD* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::ADD>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::ADD>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::ADD*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::ADD*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::ADD>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::MINUS> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::MINUS*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::MINUS*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::MINUS& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::MINUS* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::MINUS>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::MINUS>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::MINUS*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::MINUS*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::MINUS>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::MULTI> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::MULTI*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::MULTI*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::MULTI& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::MULTI* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::MULTI>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::MULTI>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::MULTI*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::MULTI*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::MULTI>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::DIVIDE> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::DIVIDE*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::DIVIDE*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::DIVIDE& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::DIVIDE* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::DIVIDE>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::DIVIDE>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::DIVIDE*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::DIVIDE*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::DIVIDE>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::LT> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::LT*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::LT*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::LT& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::LT* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::LT>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::LT>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::LT*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::LT*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::LT>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::LE> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::LE*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::LE*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::LE& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::LE* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::LE>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::LE>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::LE*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::LE*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::LE>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::GT> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::GT*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::GT*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::GT& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::GT* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::GT>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::GT>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::GT*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::GT*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::GT>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::GE> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::GE*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::GE*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::GE& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::GE* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::GE>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::GE>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::GE*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::GE*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::GE>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::EQ> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::EQ*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::EQ*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::EQ& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::EQ* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::EQ>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::EQ>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::EQ*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::EQ*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::EQ>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::DOT> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::DOT*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::DOT*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::DOT& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::DOT* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::DOT>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::DOT>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::DOT*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::DOT*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::DOT>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::COMMA> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::COMMA*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::COMMA*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::COMMA& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::COMMA* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::COMMA>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::COMMA>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::COMMA*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::COMMA*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::COMMA>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::COLON> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::COLON*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::COLON*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::COLON& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::COLON* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::COLON>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::COLON>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::COLON*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::COLON*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::COLON>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::SEMICOLON> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::SEMICOLON*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::SEMICOLON*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::SEMICOLON& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::SEMICOLON* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::SEMICOLON>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::SEMICOLON>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::SEMICOLON*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::SEMICOLON*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::SEMICOLON>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::PATTERN_INSERTION> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::PATTERN_INSERTION*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::PATTERN_INSERTION*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::PATTERN_INSERTION& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::PATTERN_INSERTION* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::PATTERN_INSERTION>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::PATTERN_INSERTION>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::PATTERN_INSERTION*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::PATTERN_INSERTION*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::PATTERN_INSERTION>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::VARNAME> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::VARNAME*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::VARNAME*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::VARNAME& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::VARNAME* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::VARNAME>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::VARNAME>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::VARNAME*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::VARNAME*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::VARNAME>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::NUMBER> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::NUMBER*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::NUMBER*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::NUMBER& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::NUMBER* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::NUMBER>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::NUMBER>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::NUMBER*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::NUMBER*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::NUMBER>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::DECIMAL> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::DECIMAL*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::DECIMAL*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::DECIMAL& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::DECIMAL* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::DECIMAL>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::DECIMAL>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::DECIMAL*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::DECIMAL*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::DECIMAL>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};

	template<>
	struct Access<::dupr::ast::node::ESCAPE_CHARS> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::ESCAPE_CHARS*> ts;

	public:
		Access(std::vector<const ::dupr::ast::node::ESCAPE_CHARS*> ts_) : ts(std::move(ts_))
		{
		}

		Access(const ::dupr::ast::node::ESCAPE_CHARS& t) : ts({&t})
		{
		}

		Access(const ::dupr::ast::node::ESCAPE_CHARS* t) : ts({t})
		{
		}

		Access() = default;

	public:
		Access<::dupr::ast::node::ESCAPE_CHARS>& operator[](::std::size_t index)
		{
			if (index >= ts.size())
			{
				ts.clear();
			}
			else
			{
				const auto* const copy = ts[index];
				ts.clear();
				ts.push_back(copy);
			}

			return *this;
		}

		Access<::dupr::ast::node::ESCAPE_CHARS>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
		{
			// swap if the other is larger
			if (indexBegin > indexEnd)
			{
				const auto tmp = indexBegin;
				indexBegin = indexEnd;
				indexEnd = tmp;
			}

			if (indexBegin >= ts.size())
			{
				ts.clear();
			}
			else
			{
				std::vector<const ::dupr::ast::node::ESCAPE_CHARS*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::ESCAPE_CHARS*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		Access<::dupr::ast::node::ESCAPE_CHARS>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}
	};


	
		inline Access<::dupr::ast::node::deamerreserved_star__stmt__> Access<::dupr::ast::node::program>::deamerreserved_star__stmt__()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::deamerreserved_star__stmt__>(Get<::dupr::ast::Type::deamerreserved_star__stmt__>(ts));
		}

		inline Access<::dupr::ast::node::stmt> Access<::dupr::ast::node::program>::stmt()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::stmt>(Get<::dupr::ast::Type::stmt>(ts));
		}

		inline Access<::dupr::ast::node::deamerreserved_star__stmt__> Access<::dupr::ast::node::deamerreserved_star__stmt__>::deamerreserved_star__stmt__()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::deamerreserved_star__stmt__>(Get<::dupr::ast::Type::deamerreserved_star__stmt__>(ts));
		}

		inline Access<::dupr::ast::node::stmt> Access<::dupr::ast::node::deamerreserved_star__stmt__>::stmt()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::stmt>(Get<::dupr::ast::Type::stmt>(ts));
		}

		inline Access<::dupr::ast::node::pattern_execution> Access<::dupr::ast::node::stmt>::pattern_execution()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::pattern_execution>(Get<::dupr::ast::Type::pattern_execution>(ts));
		}

		inline Access<::dupr::ast::node::pattern_constructor> Access<::dupr::ast::node::stmt>::pattern_constructor()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::pattern_constructor>(Get<::dupr::ast::Type::pattern_constructor>(ts));
		}

		inline Access<::dupr::ast::node::pattern_execution_content> Access<::dupr::ast::node::pattern_execution>::pattern_execution_content()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::pattern_execution_content>(Get<::dupr::ast::Type::pattern_execution_content>(ts));
		}

		inline Access<::dupr::ast::node::pattern_name> Access<::dupr::ast::node::pattern_execution>::pattern_name()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::pattern_name>(Get<::dupr::ast::Type::pattern_name>(ts));
		}

		inline Access<::dupr::ast::node::LEFT_BRACKET> Access<::dupr::ast::node::pattern_execution>::LEFT_BRACKET()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::LEFT_BRACKET>(Get<::dupr::ast::Type::LEFT_BRACKET>(ts));
		}

		inline Access<::dupr::ast::node::RIGHT_BRACKET> Access<::dupr::ast::node::pattern_execution>::RIGHT_BRACKET()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::RIGHT_BRACKET>(Get<::dupr::ast::Type::RIGHT_BRACKET>(ts));
		}

		inline Access<::dupr::ast::node::deamerreserved_plus__pattern_execution_content_stmt__> Access<::dupr::ast::node::pattern_execution_content>::deamerreserved_plus__pattern_execution_content_stmt__()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::deamerreserved_plus__pattern_execution_content_stmt__>(Get<::dupr::ast::Type::deamerreserved_plus__pattern_execution_content_stmt__>(ts));
		}

		inline Access<::dupr::ast::node::pattern_execution_content_stmt> Access<::dupr::ast::node::pattern_execution_content>::pattern_execution_content_stmt()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::pattern_execution_content_stmt>(Get<::dupr::ast::Type::pattern_execution_content_stmt>(ts));
		}

		inline Access<::dupr::ast::node::deamerreserved_plus__pattern_execution_content_stmt__> Access<::dupr::ast::node::deamerreserved_plus__pattern_execution_content_stmt__>::deamerreserved_plus__pattern_execution_content_stmt__()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::deamerreserved_plus__pattern_execution_content_stmt__>(Get<::dupr::ast::Type::deamerreserved_plus__pattern_execution_content_stmt__>(ts));
		}

		inline Access<::dupr::ast::node::pattern_execution_content_stmt> Access<::dupr::ast::node::deamerreserved_plus__pattern_execution_content_stmt__>::pattern_execution_content_stmt()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::pattern_execution_content_stmt>(Get<::dupr::ast::Type::pattern_execution_content_stmt>(ts));
		}

		inline Access<::dupr::ast::node::pattern_constructor_content_stmt> Access<::dupr::ast::node::pattern_execution_content_stmt>::pattern_constructor_content_stmt()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::pattern_constructor_content_stmt>(Get<::dupr::ast::Type::pattern_constructor_content_stmt>(ts));
		}

		inline Access<::dupr::ast::node::pattern_type> Access<::dupr::ast::node::pattern_constructor>::pattern_type()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::pattern_type>(Get<::dupr::ast::Type::pattern_type>(ts));
		}

		inline Access<::dupr::ast::node::pattern_name> Access<::dupr::ast::node::pattern_constructor>::pattern_name()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::pattern_name>(Get<::dupr::ast::Type::pattern_name>(ts));
		}

		inline Access<::dupr::ast::node::pattern_constructor_content> Access<::dupr::ast::node::pattern_constructor>::pattern_constructor_content()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::pattern_constructor_content>(Get<::dupr::ast::Type::pattern_constructor_content>(ts));
		}

		inline Access<::dupr::ast::node::LEFT_BRACKET> Access<::dupr::ast::node::pattern_constructor>::LEFT_BRACKET()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::LEFT_BRACKET>(Get<::dupr::ast::Type::LEFT_BRACKET>(ts));
		}

		inline Access<::dupr::ast::node::RIGHT_BRACKET> Access<::dupr::ast::node::pattern_constructor>::RIGHT_BRACKET()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::RIGHT_BRACKET>(Get<::dupr::ast::Type::RIGHT_BRACKET>(ts));
		}

		inline Access<::dupr::ast::node::COLON> Access<::dupr::ast::node::pattern_constructor>::COLON()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::COLON>(Get<::dupr::ast::Type::COLON>(ts));
		}

		inline Access<::dupr::ast::node::VARNAME> Access<::dupr::ast::node::pattern_type>::VARNAME()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::VARNAME>(Get<::dupr::ast::Type::VARNAME>(ts));
		}

		inline Access<::dupr::ast::node::VARNAME> Access<::dupr::ast::node::pattern_name>::VARNAME()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::VARNAME>(Get<::dupr::ast::Type::VARNAME>(ts));
		}

		inline Access<::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__> Access<::dupr::ast::node::pattern_constructor_content>::deamerreserved_plus__pattern_constructor_content_stmt__()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__>(Get<::dupr::ast::Type::deamerreserved_plus__pattern_constructor_content_stmt__>(ts));
		}

		inline Access<::dupr::ast::node::pattern_constructor_content_stmt> Access<::dupr::ast::node::pattern_constructor_content>::pattern_constructor_content_stmt()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::pattern_constructor_content_stmt>(Get<::dupr::ast::Type::pattern_constructor_content_stmt>(ts));
		}

		inline Access<::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__> Access<::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__>::deamerreserved_plus__pattern_constructor_content_stmt__()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__>(Get<::dupr::ast::Type::deamerreserved_plus__pattern_constructor_content_stmt__>(ts));
		}

		inline Access<::dupr::ast::node::pattern_constructor_content_stmt> Access<::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__>::pattern_constructor_content_stmt()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::pattern_constructor_content_stmt>(Get<::dupr::ast::Type::pattern_constructor_content_stmt>(ts));
		}

		inline Access<::dupr::ast::node::pattern_constructor_operator> Access<::dupr::ast::node::pattern_constructor_content_stmt>::pattern_constructor_operator()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::pattern_constructor_operator>(Get<::dupr::ast::Type::pattern_constructor_operator>(ts));
		}

		inline Access<::dupr::ast::node::pattern_constructor_structure> Access<::dupr::ast::node::pattern_constructor_content_stmt>::pattern_constructor_structure()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::pattern_constructor_structure>(Get<::dupr::ast::Type::pattern_constructor_structure>(ts));
		}

		inline Access<::dupr::ast::node::pattern_constructor_terminate> Access<::dupr::ast::node::pattern_constructor_content_stmt>::pattern_constructor_terminate()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::pattern_constructor_terminate>(Get<::dupr::ast::Type::pattern_constructor_terminate>(ts));
		}

		inline Access<::dupr::ast::node::pattern_constructor_encapsulation> Access<::dupr::ast::node::pattern_constructor_content_stmt>::pattern_constructor_encapsulation()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::pattern_constructor_encapsulation>(Get<::dupr::ast::Type::pattern_constructor_encapsulation>(ts));
		}

		inline Access<::dupr::ast::node::ADD> Access<::dupr::ast::node::pattern_constructor_operator>::ADD()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::ADD>(Get<::dupr::ast::Type::ADD>(ts));
		}

		inline Access<::dupr::ast::node::MINUS> Access<::dupr::ast::node::pattern_constructor_operator>::MINUS()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::MINUS>(Get<::dupr::ast::Type::MINUS>(ts));
		}

		inline Access<::dupr::ast::node::MULTI> Access<::dupr::ast::node::pattern_constructor_operator>::MULTI()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::MULTI>(Get<::dupr::ast::Type::MULTI>(ts));
		}

		inline Access<::dupr::ast::node::DIVIDE> Access<::dupr::ast::node::pattern_constructor_operator>::DIVIDE()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::DIVIDE>(Get<::dupr::ast::Type::DIVIDE>(ts));
		}

		inline Access<::dupr::ast::node::LT> Access<::dupr::ast::node::pattern_constructor_operator>::LT()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::LT>(Get<::dupr::ast::Type::LT>(ts));
		}

		inline Access<::dupr::ast::node::LE> Access<::dupr::ast::node::pattern_constructor_operator>::LE()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::LE>(Get<::dupr::ast::Type::LE>(ts));
		}

		inline Access<::dupr::ast::node::GT> Access<::dupr::ast::node::pattern_constructor_operator>::GT()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::GT>(Get<::dupr::ast::Type::GT>(ts));
		}

		inline Access<::dupr::ast::node::GE> Access<::dupr::ast::node::pattern_constructor_operator>::GE()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::GE>(Get<::dupr::ast::Type::GE>(ts));
		}

		inline Access<::dupr::ast::node::EQ> Access<::dupr::ast::node::pattern_constructor_operator>::EQ()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::EQ>(Get<::dupr::ast::Type::EQ>(ts));
		}

		inline Access<::dupr::ast::node::DOT> Access<::dupr::ast::node::pattern_constructor_structure>::DOT()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::DOT>(Get<::dupr::ast::Type::DOT>(ts));
		}

		inline Access<::dupr::ast::node::COMMA> Access<::dupr::ast::node::pattern_constructor_structure>::COMMA()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::COMMA>(Get<::dupr::ast::Type::COMMA>(ts));
		}

		inline Access<::dupr::ast::node::COLON> Access<::dupr::ast::node::pattern_constructor_structure>::COLON()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::COLON>(Get<::dupr::ast::Type::COLON>(ts));
		}

		inline Access<::dupr::ast::node::SEMICOLON> Access<::dupr::ast::node::pattern_constructor_structure>::SEMICOLON()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::SEMICOLON>(Get<::dupr::ast::Type::SEMICOLON>(ts));
		}

		inline Access<::dupr::ast::node::PATTERN_INSERTION> Access<::dupr::ast::node::pattern_constructor_terminate>::PATTERN_INSERTION()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::PATTERN_INSERTION>(Get<::dupr::ast::Type::PATTERN_INSERTION>(ts));
		}

		inline Access<::dupr::ast::node::VARNAME> Access<::dupr::ast::node::pattern_constructor_terminate>::VARNAME()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::VARNAME>(Get<::dupr::ast::Type::VARNAME>(ts));
		}

		inline Access<::dupr::ast::node::NUMBER> Access<::dupr::ast::node::pattern_constructor_terminate>::NUMBER()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::NUMBER>(Get<::dupr::ast::Type::NUMBER>(ts));
		}

		inline Access<::dupr::ast::node::DECIMAL> Access<::dupr::ast::node::pattern_constructor_terminate>::DECIMAL()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::DECIMAL>(Get<::dupr::ast::Type::DECIMAL>(ts));
		}

		inline Access<::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__> Access<::dupr::ast::node::pattern_constructor_encapsulation>::deamerreserved_plus__pattern_constructor_content_stmt__()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::deamerreserved_plus__pattern_constructor_content_stmt__>(Get<::dupr::ast::Type::deamerreserved_plus__pattern_constructor_content_stmt__>(ts));
		}

		inline Access<::dupr::ast::node::pattern_constructor_content_stmt> Access<::dupr::ast::node::pattern_constructor_encapsulation>::pattern_constructor_content_stmt()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::pattern_constructor_content_stmt>(Get<::dupr::ast::Type::pattern_constructor_content_stmt>(ts));
		}

		inline Access<::dupr::ast::node::LEFT_BRACKET> Access<::dupr::ast::node::pattern_constructor_encapsulation>::LEFT_BRACKET()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::LEFT_BRACKET>(Get<::dupr::ast::Type::LEFT_BRACKET>(ts));
		}

		inline Access<::dupr::ast::node::RIGHT_BRACKET> Access<::dupr::ast::node::pattern_constructor_encapsulation>::RIGHT_BRACKET()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::RIGHT_BRACKET>(Get<::dupr::ast::Type::RIGHT_BRACKET>(ts));
		}

		inline Access<::dupr::ast::node::LEFT_PARANTHESIS> Access<::dupr::ast::node::pattern_constructor_encapsulation>::LEFT_PARANTHESIS()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::LEFT_PARANTHESIS>(Get<::dupr::ast::Type::LEFT_PARANTHESIS>(ts));
		}

		inline Access<::dupr::ast::node::RIGHT_PARANTHESIS> Access<::dupr::ast::node::pattern_constructor_encapsulation>::RIGHT_PARANTHESIS()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::RIGHT_PARANTHESIS>(Get<::dupr::ast::Type::RIGHT_PARANTHESIS>(ts));
		}

		inline Access<::dupr::ast::node::LEFT_SQUARE_BRACKET> Access<::dupr::ast::node::pattern_constructor_encapsulation>::LEFT_SQUARE_BRACKET()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::LEFT_SQUARE_BRACKET>(Get<::dupr::ast::Type::LEFT_SQUARE_BRACKET>(ts));
		}

		inline Access<::dupr::ast::node::RIGHT_SQUARE_BRACKET> Access<::dupr::ast::node::pattern_constructor_encapsulation>::RIGHT_SQUARE_BRACKET()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return Access<::dupr::ast::node::RIGHT_SQUARE_BRACKET>(Get<::dupr::ast::Type::RIGHT_SQUARE_BRACKET>(ts));
		}


	/*!	\class Access_if
	 *
	 *	\brief Accesses the node if it is of the given type.
	 *
	 *	\note A lambda has to be supplied as second argument with paramater: "Access<T>".
	 */
	template<typename T>
	struct Access_if
	{
		const ::deamer::external::cpp::ast::Node* node;

		template<typename FunctionType>
		Access_if(const ::deamer::external::cpp::ast::Node* node_, FunctionType function) : node(node_)
		{
			if (node->GetType() == static_cast<::std::size_t>(::dupr::ast::relation::NodeTypeToEnum_v<T>))
			{
				function(Access<T>(static_cast<const T*>(node)));
			}
		}
	};

}}}

#endif // DUPR_AST_REFERENCE_ACCESS_H