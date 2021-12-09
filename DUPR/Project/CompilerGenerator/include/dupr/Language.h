#ifndef DUPR_LANGUAGE_H
#define DUPR_LANGUAGE_H
#include "Deamer/Language/Generator/Definition/Language.h"
#include "dupr/Threat.h"
#include "dupr/Identity.h"
#include "dupr/Generation.h"
#include "dupr/Grammar.h"
#include "dupr/Lexicon.h"

namespace dupr
{
	/*!	\class Language
	 *
	 *	\brief This contains the Language Definition of the language dupr
	 *
	 *	\details The LD initializes all base LPD's with its own pointer (this).
	 *	This allows the LPD's to access other LPD's via the language object.
	 *
	 *	\note This is auto-generated via the DLDL definition.
	 */
	class Language : public ::deamer::language::generator::definition::Language<
						 ::dupr::Language , ::dupr::Threat
, ::dupr::Identity
, ::dupr::Generation
, ::dupr::Grammar
, ::dupr::Lexicon
>
						 , public ::dupr::Threat
, public ::dupr::Identity
, public ::dupr::Generation
, public ::dupr::Grammar
, public ::dupr::Lexicon

	{
	public:
		Language() : ::deamer::language::generator::definition::Language<
						 ::dupr::Language , ::dupr::Threat
, ::dupr::Identity
, ::dupr::Generation
, ::dupr::Grammar
, ::dupr::Lexicon
>()
		, ::dupr::Threat::Threat(this)
, ::dupr::Identity::Identity(this)
, ::dupr::Generation::Generation(this)
, ::dupr::Grammar::Grammar(this)
, ::dupr::Lexicon::Lexicon(this)

		{
		}
		~Language() override = default;
	};
}
#endif // DUPR_LANGUAGE_H
