#ifndef DUPR_AST_REFERENCE_ACCESSTEMPLATEBASE_H
#define DUPR_AST_REFERENCE_ACCESSTEMPLATEBASE_H

#include "dupr/Ast/Relation/NodeEnumToType.h"
#include "dupr/Ast/Relation/NodeTypeToEnum.h"
#include "dupr/Ast/Relation/NodeIsInlined.h"

#include "dupr/Ast/Enum/Type.h"
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

	/*!	\class AccessTemplateBase
	 *
	 *	\brief Used to access AST nodes. It contains various helper functions to ease navigation through AST nodes.
	 *
	 *	\details This class contains the type dependent implementation of Access<T>.
	 *	Refrain from using this class, as there is no backwards compatibility
	 *	guarantee of this implementation class,
	 *	Use Access<T> instead, this is backwards compatible and offers different benefits.
	 *
	 *	\see Access
	 */
	template<typename T>
	struct AccessTemplateBase : public AccessBase
	{
		AccessTemplateBase() = delete;
		~AccessTemplateBase() = delete;
	};

	/*! \class Access
	 *
	 *	\brief Used to access AST nodes. It contains various helper functions to ease navigation through AST nodes.
	 *
	 *	\details Type dispatcher for logic.
	 *
	 *	\see AccessTemplateBase
	 */
	template<typename T>
	struct Access : public AccessTemplateBase<T>
	{
		Access(std::vector<const T*> ts_) : AccessTemplateBase<T>(ts_)
		{
		}

		Access(const T& t) : AccessTemplateBase<T>(t)
		{
		}

		Access(const T* t) : AccessTemplateBase<T>(t)
		{
		}

		Access(const AccessTemplateBase<T>& rhs) : AccessTemplateBase<T>(rhs)
		{
		}

		Access() = default;
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::program>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::deamerreserved_star__stmt__>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::stmt>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::pattern_execution>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::pattern_constructor_array>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::pattern_constructor>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::pattern_type>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::pattern_name>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::deamerreserved_arrow__VARNAME__>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::deamerreserved_star__GT__VARNAME__>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::pattern_constructor_content>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::pattern_constructor_content_stmt>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::pattern_constructor_structure>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::pattern_constructor_terminate>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::pattern_constructor_encapsulation>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::LEFT_BRACKET>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::RIGHT_BRACKET>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::LEFT_PARANTHESIS>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::RIGHT_PARANTHESIS>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::LEFT_SQUARE_BRACKET>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::RIGHT_SQUARE_BRACKET>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::ADD>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::MINUS>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::MULTI>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::DIVIDE>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::LT>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::LE>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::GT>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::GE>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::EQ>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::EQEQ>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::EQEQEQ>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::OR>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::AND>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::OROR>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::ANDAND>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::WILDCARD_OP>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::DOT>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::COMMA>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::COLON>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::SEMICOLON>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::SIGN>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::HEKJE>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::QUESTION>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::EXCLAM>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::PATTERN_INSERTION>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::VARNAME>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::NUMBER>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::DECIMAL>;
	template<>
	struct AccessTemplateBase<::dupr::ast::node::ESCAPE_CHARS>;


	
	template<>
	struct AccessTemplateBase<::dupr::ast::node::program> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::program*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::program*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::program& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::program* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::program>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::program>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::deamerreserved_star__stmt__> deamerreserved_star__stmt__();
AccessTemplateBase<::dupr::ast::node::stmt> stmt();


		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::program>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::deamerreserved_star__stmt__> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::deamerreserved_star__stmt__*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::deamerreserved_star__stmt__*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::deamerreserved_star__stmt__& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::deamerreserved_star__stmt__* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::deamerreserved_star__stmt__>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::deamerreserved_star__stmt__>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::deamerreserved_star__stmt__> deamerreserved_star__stmt__();
AccessTemplateBase<::dupr::ast::node::stmt> stmt();


		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::deamerreserved_star__stmt__>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::stmt> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::stmt*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::stmt*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::stmt& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::stmt* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::stmt>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::stmt>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::pattern_execution> pattern_execution();
AccessTemplateBase<::dupr::ast::node::pattern_constructor_array> pattern_constructor_array();
AccessTemplateBase<::dupr::ast::node::pattern_constructor> pattern_constructor();


		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::stmt>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::pattern_execution> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::pattern_execution*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::pattern_execution*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::pattern_execution& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::pattern_execution* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::pattern_execution>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::pattern_execution>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::pattern_name> pattern_name();
AccessTemplateBase<::dupr::ast::node::pattern_constructor_content> pattern_constructor_content();
AccessTemplateBase<::dupr::ast::node::LEFT_BRACKET> LEFT_BRACKET();
AccessTemplateBase<::dupr::ast::node::RIGHT_BRACKET> RIGHT_BRACKET();


		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::pattern_execution>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::pattern_constructor_array> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::pattern_constructor_array*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::pattern_constructor_array*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::pattern_constructor_array& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::pattern_constructor_array* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::pattern_constructor_array>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::pattern_constructor_array>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
				std::vector<const ::dupr::ast::node::pattern_constructor_array*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::pattern_constructor_array*> GetContent()
		{
			return ts;
		}

	public:
		AccessTemplateBase<::dupr::ast::node::pattern_constructor_array> pattern_constructor_array();
AccessTemplateBase<::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____> deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____();
AccessTemplateBase<::dupr::ast::node::pattern_constructor> pattern_constructor();
AccessTemplateBase<::dupr::ast::node::pattern_type> pattern_type();
AccessTemplateBase<::dupr::ast::node::pattern_name> pattern_name();
AccessTemplateBase<::dupr::ast::node::LEFT_SQUARE_BRACKET> LEFT_SQUARE_BRACKET();
AccessTemplateBase<::dupr::ast::node::RIGHT_SQUARE_BRACKET> RIGHT_SQUARE_BRACKET();
AccessTemplateBase<::dupr::ast::node::COLON> COLON();


		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::pattern_constructor_array>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
				std::vector<const ::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____*> GetContent()
		{
			return ts;
		}

	public:
		AccessTemplateBase<::dupr::ast::node::pattern_constructor_array> pattern_constructor_array();
AccessTemplateBase<::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____> deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____();
AccessTemplateBase<::dupr::ast::node::pattern_constructor> pattern_constructor();


		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::pattern_constructor> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::pattern_constructor*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::pattern_constructor*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::pattern_constructor& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::pattern_constructor* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::pattern_constructor>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::pattern_constructor>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::pattern_type> pattern_type();
AccessTemplateBase<::dupr::ast::node::pattern_name> pattern_name();
AccessTemplateBase<::dupr::ast::node::pattern_constructor_content> pattern_constructor_content();
AccessTemplateBase<::dupr::ast::node::LEFT_BRACKET> LEFT_BRACKET();
AccessTemplateBase<::dupr::ast::node::RIGHT_BRACKET> RIGHT_BRACKET();
AccessTemplateBase<::dupr::ast::node::COLON> COLON();


		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::pattern_constructor>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::pattern_type> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::pattern_type*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::pattern_type*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::pattern_type& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::pattern_type* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::pattern_type>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::pattern_type>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::VARNAME> VARNAME();


		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::pattern_type>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::pattern_name> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::pattern_name*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::pattern_name*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::pattern_name& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::pattern_name* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::pattern_name>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::pattern_name>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::deamerreserved_arrow__VARNAME__> deamerreserved_arrow__VARNAME__();
AccessTemplateBase<::dupr::ast::node::deamerreserved_star__GT__VARNAME__> deamerreserved_star__GT__VARNAME__();
AccessTemplateBase<::dupr::ast::node::GT> GT();
AccessTemplateBase<::dupr::ast::node::VARNAME> VARNAME();


		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::pattern_name>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::deamerreserved_arrow__VARNAME__> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::deamerreserved_arrow__VARNAME__*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::deamerreserved_arrow__VARNAME__*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::deamerreserved_arrow__VARNAME__& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::deamerreserved_arrow__VARNAME__* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::deamerreserved_arrow__VARNAME__>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::deamerreserved_arrow__VARNAME__>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
				std::vector<const ::dupr::ast::node::deamerreserved_arrow__VARNAME__*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::deamerreserved_arrow__VARNAME__*> GetContent()
		{
			return ts;
		}

	public:
		AccessTemplateBase<::dupr::ast::node::deamerreserved_star__GT__VARNAME__> deamerreserved_star__GT__VARNAME__();
AccessTemplateBase<::dupr::ast::node::GT> GT();
AccessTemplateBase<::dupr::ast::node::VARNAME> VARNAME();


		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::deamerreserved_arrow__VARNAME__>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::deamerreserved_star__GT__VARNAME__> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::deamerreserved_star__GT__VARNAME__*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::deamerreserved_star__GT__VARNAME__*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::deamerreserved_star__GT__VARNAME__& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::deamerreserved_star__GT__VARNAME__* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::deamerreserved_star__GT__VARNAME__>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::deamerreserved_star__GT__VARNAME__>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
				std::vector<const ::dupr::ast::node::deamerreserved_star__GT__VARNAME__*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::deamerreserved_star__GT__VARNAME__*> GetContent()
		{
			return ts;
		}

	public:
		AccessTemplateBase<::dupr::ast::node::deamerreserved_star__GT__VARNAME__> deamerreserved_star__GT__VARNAME__();
AccessTemplateBase<::dupr::ast::node::GT> GT();
AccessTemplateBase<::dupr::ast::node::VARNAME> VARNAME();


		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::deamerreserved_star__GT__VARNAME__>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::pattern_constructor_content> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::pattern_constructor_content*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::pattern_constructor_content*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::pattern_constructor_content& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::pattern_constructor_content* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::pattern_constructor_content>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::pattern_constructor_content>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__> deamerreserved_star__pattern_constructor_content_stmt__();
AccessTemplateBase<::dupr::ast::node::pattern_constructor_content_stmt> pattern_constructor_content_stmt();


		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::pattern_constructor_content>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
				std::vector<const ::dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__*> GetContent()
		{
			return ts;
		}

	public:
		AccessTemplateBase<::dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__> deamerreserved_star__pattern_constructor_content_stmt__();
AccessTemplateBase<::dupr::ast::node::pattern_constructor_content_stmt> pattern_constructor_content_stmt();


		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::pattern_constructor_content_stmt> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::pattern_constructor_content_stmt*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::pattern_constructor_content_stmt*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::pattern_constructor_content_stmt& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::pattern_constructor_content_stmt* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::pattern_constructor_content_stmt>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::pattern_constructor_content_stmt>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator> pattern_constructor_operator();
AccessTemplateBase<::dupr::ast::node::pattern_constructor_structure> pattern_constructor_structure();
AccessTemplateBase<::dupr::ast::node::pattern_constructor_terminate> pattern_constructor_terminate();
AccessTemplateBase<::dupr::ast::node::pattern_constructor_encapsulation> pattern_constructor_encapsulation();


		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::pattern_constructor_content_stmt>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::pattern_constructor_operator*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::pattern_constructor_operator*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::pattern_constructor_operator& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::pattern_constructor_operator* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::ADD> ADD();
AccessTemplateBase<::dupr::ast::node::MINUS> MINUS();
AccessTemplateBase<::dupr::ast::node::MULTI> MULTI();
AccessTemplateBase<::dupr::ast::node::DIVIDE> DIVIDE();
AccessTemplateBase<::dupr::ast::node::LT> LT();
AccessTemplateBase<::dupr::ast::node::LE> LE();
AccessTemplateBase<::dupr::ast::node::GT> GT();
AccessTemplateBase<::dupr::ast::node::GE> GE();
AccessTemplateBase<::dupr::ast::node::EQ> EQ();
AccessTemplateBase<::dupr::ast::node::EQEQ> EQEQ();
AccessTemplateBase<::dupr::ast::node::EQEQEQ> EQEQEQ();
AccessTemplateBase<::dupr::ast::node::OR> OR();
AccessTemplateBase<::dupr::ast::node::AND> AND();
AccessTemplateBase<::dupr::ast::node::OROR> OROR();
AccessTemplateBase<::dupr::ast::node::ANDAND> ANDAND();
AccessTemplateBase<::dupr::ast::node::WILDCARD_OP> WILDCARD_OP();


		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::pattern_constructor_structure> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::pattern_constructor_structure*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::pattern_constructor_structure*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::pattern_constructor_structure& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::pattern_constructor_structure* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::pattern_constructor_structure>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::pattern_constructor_structure>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::DOT> DOT();
AccessTemplateBase<::dupr::ast::node::COMMA> COMMA();
AccessTemplateBase<::dupr::ast::node::COLON> COLON();
AccessTemplateBase<::dupr::ast::node::SEMICOLON> SEMICOLON();
AccessTemplateBase<::dupr::ast::node::SIGN> SIGN();
AccessTemplateBase<::dupr::ast::node::HEKJE> HEKJE();
AccessTemplateBase<::dupr::ast::node::QUESTION> QUESTION();
AccessTemplateBase<::dupr::ast::node::EXCLAM> EXCLAM();


		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::pattern_constructor_structure>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::pattern_constructor_terminate> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::pattern_constructor_terminate*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::pattern_constructor_terminate*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::pattern_constructor_terminate& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::pattern_constructor_terminate* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::pattern_constructor_terminate>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::pattern_constructor_terminate>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::PATTERN_INSERTION> PATTERN_INSERTION();
AccessTemplateBase<::dupr::ast::node::VARNAME> VARNAME();
AccessTemplateBase<::dupr::ast::node::NUMBER> NUMBER();
AccessTemplateBase<::dupr::ast::node::DECIMAL> DECIMAL();


		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::pattern_constructor_terminate>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::pattern_constructor_encapsulation> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::pattern_constructor_encapsulation*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::pattern_constructor_encapsulation*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::pattern_constructor_encapsulation& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::pattern_constructor_encapsulation* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::pattern_constructor_encapsulation>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::pattern_constructor_encapsulation>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::pattern_constructor_content> pattern_constructor_content();
AccessTemplateBase<::dupr::ast::node::LEFT_BRACKET> LEFT_BRACKET();
AccessTemplateBase<::dupr::ast::node::RIGHT_BRACKET> RIGHT_BRACKET();
AccessTemplateBase<::dupr::ast::node::LEFT_PARANTHESIS> LEFT_PARANTHESIS();
AccessTemplateBase<::dupr::ast::node::RIGHT_PARANTHESIS> RIGHT_PARANTHESIS();
AccessTemplateBase<::dupr::ast::node::LEFT_SQUARE_BRACKET> LEFT_SQUARE_BRACKET();
AccessTemplateBase<::dupr::ast::node::RIGHT_SQUARE_BRACKET> RIGHT_SQUARE_BRACKET();


		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::pattern_constructor_encapsulation>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::LEFT_BRACKET> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::LEFT_BRACKET*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::LEFT_BRACKET*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::LEFT_BRACKET& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::LEFT_BRACKET* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::LEFT_BRACKET>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::LEFT_BRACKET>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::LEFT_BRACKET>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::RIGHT_BRACKET> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::RIGHT_BRACKET*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::RIGHT_BRACKET*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::RIGHT_BRACKET& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::RIGHT_BRACKET* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::RIGHT_BRACKET>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::RIGHT_BRACKET>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::RIGHT_BRACKET>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::LEFT_PARANTHESIS> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::LEFT_PARANTHESIS*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::LEFT_PARANTHESIS*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::LEFT_PARANTHESIS& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::LEFT_PARANTHESIS* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::LEFT_PARANTHESIS>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::LEFT_PARANTHESIS>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::LEFT_PARANTHESIS>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::RIGHT_PARANTHESIS> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::RIGHT_PARANTHESIS*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::RIGHT_PARANTHESIS*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::RIGHT_PARANTHESIS& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::RIGHT_PARANTHESIS* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::RIGHT_PARANTHESIS>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::RIGHT_PARANTHESIS>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::RIGHT_PARANTHESIS>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::LEFT_SQUARE_BRACKET> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::LEFT_SQUARE_BRACKET*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::LEFT_SQUARE_BRACKET*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::LEFT_SQUARE_BRACKET& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::LEFT_SQUARE_BRACKET* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::LEFT_SQUARE_BRACKET>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::LEFT_SQUARE_BRACKET>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::LEFT_SQUARE_BRACKET>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::RIGHT_SQUARE_BRACKET> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::RIGHT_SQUARE_BRACKET*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::RIGHT_SQUARE_BRACKET*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::RIGHT_SQUARE_BRACKET& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::RIGHT_SQUARE_BRACKET* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::RIGHT_SQUARE_BRACKET>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::RIGHT_SQUARE_BRACKET>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::RIGHT_SQUARE_BRACKET>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::ADD> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::ADD*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::ADD*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::ADD& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::ADD* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::ADD>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::ADD>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::ADD>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::MINUS> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::MINUS*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::MINUS*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::MINUS& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::MINUS* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::MINUS>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::MINUS>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::MINUS>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::MULTI> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::MULTI*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::MULTI*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::MULTI& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::MULTI* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::MULTI>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::MULTI>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::MULTI>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::DIVIDE> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::DIVIDE*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::DIVIDE*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::DIVIDE& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::DIVIDE* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::DIVIDE>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::DIVIDE>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::DIVIDE>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::LT> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::LT*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::LT*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::LT& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::LT* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::LT>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::LT>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::LT>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::LE> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::LE*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::LE*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::LE& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::LE* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::LE>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::LE>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::LE>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::GT> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::GT*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::GT*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::GT& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::GT* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::GT>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::GT>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::GT>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::GE> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::GE*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::GE*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::GE& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::GE* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::GE>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::GE>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::GE>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::EQ> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::EQ*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::EQ*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::EQ& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::EQ* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::EQ>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::EQ>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::EQ>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::EQEQ> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::EQEQ*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::EQEQ*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::EQEQ& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::EQEQ* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::EQEQ>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::EQEQ>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
				std::vector<const ::dupr::ast::node::EQEQ*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::EQEQ*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::EQEQ>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::EQEQEQ> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::EQEQEQ*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::EQEQEQ*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::EQEQEQ& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::EQEQEQ* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::EQEQEQ>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::EQEQEQ>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
				std::vector<const ::dupr::ast::node::EQEQEQ*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::EQEQEQ*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::EQEQEQ>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::OR> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::OR*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::OR*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::OR& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::OR* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::OR>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::OR>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
				std::vector<const ::dupr::ast::node::OR*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::OR*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::OR>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::AND> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::AND*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::AND*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::AND& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::AND* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::AND>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::AND>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
				std::vector<const ::dupr::ast::node::AND*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::AND*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::AND>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::OROR> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::OROR*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::OROR*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::OROR& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::OROR* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::OROR>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::OROR>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
				std::vector<const ::dupr::ast::node::OROR*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::OROR*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::OROR>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::ANDAND> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::ANDAND*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::ANDAND*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::ANDAND& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::ANDAND* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::ANDAND>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::ANDAND>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
				std::vector<const ::dupr::ast::node::ANDAND*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::ANDAND*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::ANDAND>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::WILDCARD_OP> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::WILDCARD_OP*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::WILDCARD_OP*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::WILDCARD_OP& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::WILDCARD_OP* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::WILDCARD_OP>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::WILDCARD_OP>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
				std::vector<const ::dupr::ast::node::WILDCARD_OP*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::WILDCARD_OP*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::WILDCARD_OP>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::DOT> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::DOT*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::DOT*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::DOT& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::DOT* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::DOT>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::DOT>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::DOT>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::COMMA> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::COMMA*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::COMMA*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::COMMA& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::COMMA* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::COMMA>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::COMMA>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::COMMA>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::COLON> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::COLON*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::COLON*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::COLON& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::COLON* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::COLON>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::COLON>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::COLON>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::SEMICOLON> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::SEMICOLON*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::SEMICOLON*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::SEMICOLON& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::SEMICOLON* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::SEMICOLON>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::SEMICOLON>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::SEMICOLON>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::SIGN> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::SIGN*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::SIGN*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::SIGN& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::SIGN* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::SIGN>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::SIGN>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
				std::vector<const ::dupr::ast::node::SIGN*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::SIGN*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::SIGN>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::HEKJE> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::HEKJE*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::HEKJE*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::HEKJE& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::HEKJE* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::HEKJE>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::HEKJE>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
				std::vector<const ::dupr::ast::node::HEKJE*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::HEKJE*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::HEKJE>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::QUESTION> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::QUESTION*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::QUESTION*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::QUESTION& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::QUESTION* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::QUESTION>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::QUESTION>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
				std::vector<const ::dupr::ast::node::QUESTION*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::QUESTION*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::QUESTION>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::EXCLAM> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::EXCLAM*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::EXCLAM*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::EXCLAM& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::EXCLAM* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::EXCLAM>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::EXCLAM>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
				std::vector<const ::dupr::ast::node::EXCLAM*> temporaries;
				for (auto i = indexBegin; i < ts.size() && i <= indexEnd; i++)
				{
					temporaries.push_back(ts[i]);
				}
				ts.clear();
				ts = temporaries;
			}

			return *this;
		}

		std::vector<const ::dupr::ast::node::EXCLAM*> GetContent()
		{
			return ts;
		}

	public:
		

		template<typename FunctionType>
		AccessTemplateBase<::dupr::ast::node::EXCLAM>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::PATTERN_INSERTION> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::PATTERN_INSERTION*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::PATTERN_INSERTION*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::PATTERN_INSERTION& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::PATTERN_INSERTION* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::PATTERN_INSERTION>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::PATTERN_INSERTION>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::PATTERN_INSERTION>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::VARNAME> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::VARNAME*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::VARNAME*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::VARNAME& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::VARNAME* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::VARNAME>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::VARNAME>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::VARNAME>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::NUMBER> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::NUMBER*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::NUMBER*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::NUMBER& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::NUMBER* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::NUMBER>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::NUMBER>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::NUMBER>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::DECIMAL> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::DECIMAL*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::DECIMAL*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::DECIMAL& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::DECIMAL* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::DECIMAL>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::DECIMAL>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::DECIMAL>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};

	template<>
	struct AccessTemplateBase<::dupr::ast::node::ESCAPE_CHARS> : public AccessBase
	{
	protected:
		std::vector<const ::dupr::ast::node::ESCAPE_CHARS*> ts;

	public:
		AccessTemplateBase(std::vector<const ::dupr::ast::node::ESCAPE_CHARS*> ts_) : ts(std::move(ts_))
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::ESCAPE_CHARS& t) : ts({&t})
		{
		}

		AccessTemplateBase(const ::dupr::ast::node::ESCAPE_CHARS* t) : ts({t})
		{
		}

		AccessTemplateBase() = default;

	public:
		AccessTemplateBase<::dupr::ast::node::ESCAPE_CHARS>& operator[](::std::size_t index)
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

		AccessTemplateBase<::dupr::ast::node::ESCAPE_CHARS>& operator()(::std::size_t indexBegin, ::std::size_t indexEnd)
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
		AccessTemplateBase<::dupr::ast::node::ESCAPE_CHARS>& for_all(FunctionType function)
		{
			for (const auto* const t : ts)
			{
				function(t);
			}

			return *this;
		}

	public:
		auto begin()
		{
			return ts.begin();
		}
		auto cbegin()
		{
			return ts.cbegin();
		}
		
		auto end()
		{
			return ts.end();
		}
		
		auto cend()
		{
			return ts.cend();
		}
	};


	
		inline AccessTemplateBase<::dupr::ast::node::deamerreserved_star__stmt__> AccessTemplateBase<::dupr::ast::node::program>::deamerreserved_star__stmt__()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::deamerreserved_star__stmt__>(Get<::dupr::ast::Type::deamerreserved_star__stmt__>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::stmt> AccessTemplateBase<::dupr::ast::node::program>::stmt()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::stmt>(Get<::dupr::ast::Type::stmt>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::deamerreserved_star__stmt__> AccessTemplateBase<::dupr::ast::node::deamerreserved_star__stmt__>::deamerreserved_star__stmt__()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::deamerreserved_star__stmt__>(Get<::dupr::ast::Type::deamerreserved_star__stmt__>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::stmt> AccessTemplateBase<::dupr::ast::node::deamerreserved_star__stmt__>::stmt()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::stmt>(Get<::dupr::ast::Type::stmt>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::pattern_execution> AccessTemplateBase<::dupr::ast::node::stmt>::pattern_execution()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::pattern_execution>(Get<::dupr::ast::Type::pattern_execution>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::pattern_constructor_array> AccessTemplateBase<::dupr::ast::node::stmt>::pattern_constructor_array()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::pattern_constructor_array>(Get<::dupr::ast::Type::pattern_constructor_array>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::pattern_constructor> AccessTemplateBase<::dupr::ast::node::stmt>::pattern_constructor()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::pattern_constructor>(Get<::dupr::ast::Type::pattern_constructor>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::pattern_name> AccessTemplateBase<::dupr::ast::node::pattern_execution>::pattern_name()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::pattern_name>(Get<::dupr::ast::Type::pattern_name>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::pattern_constructor_content> AccessTemplateBase<::dupr::ast::node::pattern_execution>::pattern_constructor_content()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::pattern_constructor_content>(Get<::dupr::ast::Type::pattern_constructor_content>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::LEFT_BRACKET> AccessTemplateBase<::dupr::ast::node::pattern_execution>::LEFT_BRACKET()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::LEFT_BRACKET>(Get<::dupr::ast::Type::LEFT_BRACKET>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::RIGHT_BRACKET> AccessTemplateBase<::dupr::ast::node::pattern_execution>::RIGHT_BRACKET()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::RIGHT_BRACKET>(Get<::dupr::ast::Type::RIGHT_BRACKET>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::pattern_constructor_array> AccessTemplateBase<::dupr::ast::node::pattern_constructor_array>::pattern_constructor_array()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::pattern_constructor_array>(Get<::dupr::ast::Type::pattern_constructor_array>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____> AccessTemplateBase<::dupr::ast::node::pattern_constructor_array>::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____>(Get<::dupr::ast::Type::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::pattern_constructor> AccessTemplateBase<::dupr::ast::node::pattern_constructor_array>::pattern_constructor()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::pattern_constructor>(Get<::dupr::ast::Type::pattern_constructor>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::pattern_type> AccessTemplateBase<::dupr::ast::node::pattern_constructor_array>::pattern_type()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::pattern_type>(Get<::dupr::ast::Type::pattern_type>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::pattern_name> AccessTemplateBase<::dupr::ast::node::pattern_constructor_array>::pattern_name()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::pattern_name>(Get<::dupr::ast::Type::pattern_name>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::LEFT_SQUARE_BRACKET> AccessTemplateBase<::dupr::ast::node::pattern_constructor_array>::LEFT_SQUARE_BRACKET()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::LEFT_SQUARE_BRACKET>(Get<::dupr::ast::Type::LEFT_SQUARE_BRACKET>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::RIGHT_SQUARE_BRACKET> AccessTemplateBase<::dupr::ast::node::pattern_constructor_array>::RIGHT_SQUARE_BRACKET()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::RIGHT_SQUARE_BRACKET>(Get<::dupr::ast::Type::RIGHT_SQUARE_BRACKET>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::COLON> AccessTemplateBase<::dupr::ast::node::pattern_constructor_array>::COLON()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::COLON>(Get<::dupr::ast::Type::COLON>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::pattern_constructor_array> AccessTemplateBase<::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____>::pattern_constructor_array()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::pattern_constructor_array>(Get<::dupr::ast::Type::pattern_constructor_array>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____> AccessTemplateBase<::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____>::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____>(Get<::dupr::ast::Type::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::pattern_constructor> AccessTemplateBase<::dupr::ast::node::deamerreserved_plus__deamerreserved_or__pattern_constructor__pattern_constructor_array____>::pattern_constructor()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::pattern_constructor>(Get<::dupr::ast::Type::pattern_constructor>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::pattern_type> AccessTemplateBase<::dupr::ast::node::pattern_constructor>::pattern_type()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::pattern_type>(Get<::dupr::ast::Type::pattern_type>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::pattern_name> AccessTemplateBase<::dupr::ast::node::pattern_constructor>::pattern_name()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::pattern_name>(Get<::dupr::ast::Type::pattern_name>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::pattern_constructor_content> AccessTemplateBase<::dupr::ast::node::pattern_constructor>::pattern_constructor_content()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::pattern_constructor_content>(Get<::dupr::ast::Type::pattern_constructor_content>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::LEFT_BRACKET> AccessTemplateBase<::dupr::ast::node::pattern_constructor>::LEFT_BRACKET()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::LEFT_BRACKET>(Get<::dupr::ast::Type::LEFT_BRACKET>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::RIGHT_BRACKET> AccessTemplateBase<::dupr::ast::node::pattern_constructor>::RIGHT_BRACKET()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::RIGHT_BRACKET>(Get<::dupr::ast::Type::RIGHT_BRACKET>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::COLON> AccessTemplateBase<::dupr::ast::node::pattern_constructor>::COLON()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::COLON>(Get<::dupr::ast::Type::COLON>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::VARNAME> AccessTemplateBase<::dupr::ast::node::pattern_type>::VARNAME()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::VARNAME>(Get<::dupr::ast::Type::VARNAME>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::deamerreserved_arrow__VARNAME__> AccessTemplateBase<::dupr::ast::node::pattern_name>::deamerreserved_arrow__VARNAME__()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::deamerreserved_arrow__VARNAME__>(Get<::dupr::ast::Type::deamerreserved_arrow__VARNAME__>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::deamerreserved_star__GT__VARNAME__> AccessTemplateBase<::dupr::ast::node::pattern_name>::deamerreserved_star__GT__VARNAME__()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::deamerreserved_star__GT__VARNAME__>(Get<::dupr::ast::Type::deamerreserved_star__GT__VARNAME__>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::GT> AccessTemplateBase<::dupr::ast::node::pattern_name>::GT()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::GT>(Get<::dupr::ast::Type::GT>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::VARNAME> AccessTemplateBase<::dupr::ast::node::pattern_name>::VARNAME()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::VARNAME>(Get<::dupr::ast::Type::VARNAME>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::deamerreserved_star__GT__VARNAME__> AccessTemplateBase<::dupr::ast::node::deamerreserved_arrow__VARNAME__>::deamerreserved_star__GT__VARNAME__()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::deamerreserved_star__GT__VARNAME__>(Get<::dupr::ast::Type::deamerreserved_star__GT__VARNAME__>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::GT> AccessTemplateBase<::dupr::ast::node::deamerreserved_arrow__VARNAME__>::GT()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::GT>(Get<::dupr::ast::Type::GT>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::VARNAME> AccessTemplateBase<::dupr::ast::node::deamerreserved_arrow__VARNAME__>::VARNAME()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::VARNAME>(Get<::dupr::ast::Type::VARNAME>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::deamerreserved_star__GT__VARNAME__> AccessTemplateBase<::dupr::ast::node::deamerreserved_star__GT__VARNAME__>::deamerreserved_star__GT__VARNAME__()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::deamerreserved_star__GT__VARNAME__>(Get<::dupr::ast::Type::deamerreserved_star__GT__VARNAME__>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::GT> AccessTemplateBase<::dupr::ast::node::deamerreserved_star__GT__VARNAME__>::GT()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::GT>(Get<::dupr::ast::Type::GT>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::VARNAME> AccessTemplateBase<::dupr::ast::node::deamerreserved_star__GT__VARNAME__>::VARNAME()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::VARNAME>(Get<::dupr::ast::Type::VARNAME>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__> AccessTemplateBase<::dupr::ast::node::pattern_constructor_content>::deamerreserved_star__pattern_constructor_content_stmt__()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__>(Get<::dupr::ast::Type::deamerreserved_star__pattern_constructor_content_stmt__>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::pattern_constructor_content_stmt> AccessTemplateBase<::dupr::ast::node::pattern_constructor_content>::pattern_constructor_content_stmt()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::pattern_constructor_content_stmt>(Get<::dupr::ast::Type::pattern_constructor_content_stmt>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__> AccessTemplateBase<::dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__>::deamerreserved_star__pattern_constructor_content_stmt__()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__>(Get<::dupr::ast::Type::deamerreserved_star__pattern_constructor_content_stmt__>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::pattern_constructor_content_stmt> AccessTemplateBase<::dupr::ast::node::deamerreserved_star__pattern_constructor_content_stmt__>::pattern_constructor_content_stmt()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::pattern_constructor_content_stmt>(Get<::dupr::ast::Type::pattern_constructor_content_stmt>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator> AccessTemplateBase<::dupr::ast::node::pattern_constructor_content_stmt>::pattern_constructor_operator()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator>(Get<::dupr::ast::Type::pattern_constructor_operator>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::pattern_constructor_structure> AccessTemplateBase<::dupr::ast::node::pattern_constructor_content_stmt>::pattern_constructor_structure()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::pattern_constructor_structure>(Get<::dupr::ast::Type::pattern_constructor_structure>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::pattern_constructor_terminate> AccessTemplateBase<::dupr::ast::node::pattern_constructor_content_stmt>::pattern_constructor_terminate()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::pattern_constructor_terminate>(Get<::dupr::ast::Type::pattern_constructor_terminate>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::pattern_constructor_encapsulation> AccessTemplateBase<::dupr::ast::node::pattern_constructor_content_stmt>::pattern_constructor_encapsulation()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::pattern_constructor_encapsulation>(Get<::dupr::ast::Type::pattern_constructor_encapsulation>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::ADD> AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator>::ADD()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::ADD>(Get<::dupr::ast::Type::ADD>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::MINUS> AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator>::MINUS()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::MINUS>(Get<::dupr::ast::Type::MINUS>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::MULTI> AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator>::MULTI()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::MULTI>(Get<::dupr::ast::Type::MULTI>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::DIVIDE> AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator>::DIVIDE()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::DIVIDE>(Get<::dupr::ast::Type::DIVIDE>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::LT> AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator>::LT()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::LT>(Get<::dupr::ast::Type::LT>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::LE> AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator>::LE()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::LE>(Get<::dupr::ast::Type::LE>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::GT> AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator>::GT()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::GT>(Get<::dupr::ast::Type::GT>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::GE> AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator>::GE()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::GE>(Get<::dupr::ast::Type::GE>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::EQ> AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator>::EQ()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::EQ>(Get<::dupr::ast::Type::EQ>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::EQEQ> AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator>::EQEQ()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::EQEQ>(Get<::dupr::ast::Type::EQEQ>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::EQEQEQ> AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator>::EQEQEQ()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::EQEQEQ>(Get<::dupr::ast::Type::EQEQEQ>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::OR> AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator>::OR()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::OR>(Get<::dupr::ast::Type::OR>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::AND> AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator>::AND()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::AND>(Get<::dupr::ast::Type::AND>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::OROR> AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator>::OROR()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::OROR>(Get<::dupr::ast::Type::OROR>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::ANDAND> AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator>::ANDAND()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::ANDAND>(Get<::dupr::ast::Type::ANDAND>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::WILDCARD_OP> AccessTemplateBase<::dupr::ast::node::pattern_constructor_operator>::WILDCARD_OP()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::WILDCARD_OP>(Get<::dupr::ast::Type::WILDCARD_OP>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::DOT> AccessTemplateBase<::dupr::ast::node::pattern_constructor_structure>::DOT()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::DOT>(Get<::dupr::ast::Type::DOT>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::COMMA> AccessTemplateBase<::dupr::ast::node::pattern_constructor_structure>::COMMA()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::COMMA>(Get<::dupr::ast::Type::COMMA>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::COLON> AccessTemplateBase<::dupr::ast::node::pattern_constructor_structure>::COLON()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::COLON>(Get<::dupr::ast::Type::COLON>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::SEMICOLON> AccessTemplateBase<::dupr::ast::node::pattern_constructor_structure>::SEMICOLON()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::SEMICOLON>(Get<::dupr::ast::Type::SEMICOLON>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::SIGN> AccessTemplateBase<::dupr::ast::node::pattern_constructor_structure>::SIGN()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::SIGN>(Get<::dupr::ast::Type::SIGN>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::HEKJE> AccessTemplateBase<::dupr::ast::node::pattern_constructor_structure>::HEKJE()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::HEKJE>(Get<::dupr::ast::Type::HEKJE>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::QUESTION> AccessTemplateBase<::dupr::ast::node::pattern_constructor_structure>::QUESTION()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::QUESTION>(Get<::dupr::ast::Type::QUESTION>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::EXCLAM> AccessTemplateBase<::dupr::ast::node::pattern_constructor_structure>::EXCLAM()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::EXCLAM>(Get<::dupr::ast::Type::EXCLAM>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::PATTERN_INSERTION> AccessTemplateBase<::dupr::ast::node::pattern_constructor_terminate>::PATTERN_INSERTION()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::PATTERN_INSERTION>(Get<::dupr::ast::Type::PATTERN_INSERTION>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::VARNAME> AccessTemplateBase<::dupr::ast::node::pattern_constructor_terminate>::VARNAME()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::VARNAME>(Get<::dupr::ast::Type::VARNAME>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::NUMBER> AccessTemplateBase<::dupr::ast::node::pattern_constructor_terminate>::NUMBER()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::NUMBER>(Get<::dupr::ast::Type::NUMBER>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::DECIMAL> AccessTemplateBase<::dupr::ast::node::pattern_constructor_terminate>::DECIMAL()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::DECIMAL>(Get<::dupr::ast::Type::DECIMAL>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::pattern_constructor_content> AccessTemplateBase<::dupr::ast::node::pattern_constructor_encapsulation>::pattern_constructor_content()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::pattern_constructor_content>(Get<::dupr::ast::Type::pattern_constructor_content>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::LEFT_BRACKET> AccessTemplateBase<::dupr::ast::node::pattern_constructor_encapsulation>::LEFT_BRACKET()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::LEFT_BRACKET>(Get<::dupr::ast::Type::LEFT_BRACKET>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::RIGHT_BRACKET> AccessTemplateBase<::dupr::ast::node::pattern_constructor_encapsulation>::RIGHT_BRACKET()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::RIGHT_BRACKET>(Get<::dupr::ast::Type::RIGHT_BRACKET>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::LEFT_PARANTHESIS> AccessTemplateBase<::dupr::ast::node::pattern_constructor_encapsulation>::LEFT_PARANTHESIS()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::LEFT_PARANTHESIS>(Get<::dupr::ast::Type::LEFT_PARANTHESIS>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::RIGHT_PARANTHESIS> AccessTemplateBase<::dupr::ast::node::pattern_constructor_encapsulation>::RIGHT_PARANTHESIS()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::RIGHT_PARANTHESIS>(Get<::dupr::ast::Type::RIGHT_PARANTHESIS>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::LEFT_SQUARE_BRACKET> AccessTemplateBase<::dupr::ast::node::pattern_constructor_encapsulation>::LEFT_SQUARE_BRACKET()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::LEFT_SQUARE_BRACKET>(Get<::dupr::ast::Type::LEFT_SQUARE_BRACKET>(ts));
		}

		inline AccessTemplateBase<::dupr::ast::node::RIGHT_SQUARE_BRACKET> AccessTemplateBase<::dupr::ast::node::pattern_constructor_encapsulation>::RIGHT_SQUARE_BRACKET()
		{
			// Optimized search, if it fails continue using unoptimized search.

			// Unoptimized search
			return AccessTemplateBase<::dupr::ast::node::RIGHT_SQUARE_BRACKET>(Get<::dupr::ast::Type::RIGHT_SQUARE_BRACKET>(ts));
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

#endif // DUPR_AST_REFERENCE_ACCESSTEMPLATEBASE_H