#ifndef DUPR_IDENTITY_H
#define DUPR_IDENTITY_H
#include "Deamer/Language/Generator/Definition/Property/User/Special/Identity.h"
namespace dupr
{
	class Language;
	/*!	\class Identity
	 *
	 *	\brief This contains the identity LPD of the language dupr
	 *
	 *	\note This is auto-generated via the DLDL definition.
	 */
	class Identity : public ::deamer::language::generator::definition::property::user::Identity<
								::dupr::Language>
	{
	public:
		::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Name> name;
	public:
		Identity(dupr::Language* language)
			:	::deamer::language::generator::definition::property::user::Identity<
					::dupr::Language>(language)
		{
		}
		void GenerateObjects() override
		{
			name.Set(::deamer::language::type::definition::object::main::Name("dupr"));
			
			AddObject(name);
			// Place higher level operations here.
			// ReplaceObject(..., ...)
			// DeleteObject(..., ...)
			
		}
	};
}
#endif // DUPR_IDENTITY_H
