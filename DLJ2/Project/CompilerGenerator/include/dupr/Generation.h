#ifndef DUPR_GENERATION_H
#define DUPR_GENERATION_H

#include "Deamer/Language/Generator/Definition/Property/User/Special/Generation.h"

namespace dupr
{
	class Language;

	/*!	\class Generation
	 *
	 *	\brief This contains the generation LPD of the language dupr
	 *
	 *	\note This is auto-generated via the DLDL definition.
	 */
	class Generation : public ::deamer::language::generator::definition::property::user::Generation<
								::dupr::Language>
	{
	public:
		::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Generate> generate_DeamerAST;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Generate> generate_Flex;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Generate> generate_Bison;

		::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Integrate> integrate_FlexAndBison;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Integrate> integrate_FlexAndDeamerAST;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Integrate> integrate_BisonAndDeamerAST;

		::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::GenerateArgument> argument_Flex_Output_terminal_objects;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::GenerateArgument> argument_Bison_Declare_deleted_terminals;

		::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::OSTarget> os_windows;
	
	public:
		Generation(dupr::Language* language)
			:	::deamer::language::generator::definition::property::user::Generation<
					::dupr::Language>(language)
		{
		}

		void GenerateObjects() override
		{
			generate_DeamerAST.Set(::deamer::language::type::definition::object::main::Generate(::deamer::tool::type::Tool::DeamerAST));
generate_Flex.Set(::deamer::language::type::definition::object::main::Generate(::deamer::tool::type::Tool::Flex));
generate_Bison.Set(::deamer::language::type::definition::object::main::Generate(::deamer::tool::type::Tool::Bison));

			integrate_FlexAndBison.Set(::deamer::language::type::definition::object::main::Integrate(::deamer::tool::type::Tool::Flex, ::deamer::tool::type::Tool::Bison));
integrate_FlexAndDeamerAST.Set(::deamer::language::type::definition::object::main::Integrate(::deamer::tool::type::Tool::Flex, ::deamer::tool::type::Tool::DeamerAST));
integrate_BisonAndDeamerAST.Set(::deamer::language::type::definition::object::main::Integrate(::deamer::tool::type::Tool::Bison, ::deamer::tool::type::Tool::DeamerAST));

			argument_Flex_Output_terminal_objects.Set(::deamer::language::type::definition::object::main::GenerateArgument(::deamer::tool::type::Tool::Flex, "Output-terminal-objects"));
argument_Bison_Declare_deleted_terminals.Set(::deamer::language::type::definition::object::main::GenerateArgument(::deamer::tool::type::Tool::Bison, "Declare-deleted-terminals"));

			os_windows.Set(::deamer::language::type::definition::object::main::OSTarget(::deamer::file::tool::OSType::os_windows));

			// Add object calls
			// AddObject(...)
			AddObject(generate_DeamerAST);
AddObject(generate_Flex);
AddObject(generate_Bison);

			AddObject(integrate_FlexAndBison);
AddObject(integrate_FlexAndDeamerAST);
AddObject(integrate_BisonAndDeamerAST);

			AddObject(argument_Flex_Output_terminal_objects);
AddObject(argument_Bison_Declare_deleted_terminals);

			AddObject(os_windows);

			// Place higher level operations here.
			// ReplaceObject(..., ...)
			// DeleteObject(..., ...)
			
		}
	};
}

#endif // DUPR_GENERATION_H
