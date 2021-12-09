#ifndef DUPR_THREAT_H
#define DUPR_THREAT_H

#include "Deamer/Language/Generator/Definition/Property/Standard/Main/Threat.h"
#include "Deamer/Language/Type/Definition/Object/Main/Threat/Analyzer/Combined/Deamer/DeamerCore.h"

namespace dupr
{
	class Language;

	/*!	\class Threat
	 *
	 *	\brief This contains the threat LPD of the language dupr
	 *
	 *	\note This is auto-generated via the DLDL definition.
	 */
	class Threat : public ::deamer::language::generator::definition::property::standard::Threat<
			  ::dupr::Language, ::deamer::language::type::definition::object::main::threat::
											analyzer::deamer::DeamerCore>
	{
	public:
		Threat(::dupr::Language* language)
			: ::deamer::language::generator::definition::property::standard::Threat<
				  ::dupr::Language, ::deamer::language::type::definition::object::main::
												threat::analyzer::deamer::DeamerCore>(language)
		{
		}
		~Threat() override = default;
	};
}

#endif // DUPR_THREAT_H
