#ifndef DUPR_LEXICON_H
#define DUPR_LEXICON_H
#include "Deamer/Language/Generator/Definition/Property/User/Main/Lexicon.h"
namespace dupr
{
	class Language;
	class Lexicon : public ::deamer::language::generator::definition::property::user::Lexicon<
								::dupr::Language>
	{
	public:
		// Terminal declarations
		::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> LEFT_BRACKET;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> RIGHT_BRACKET;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> LEFT_PARANTHESIS;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> RIGHT_PARANTHESIS;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> LEFT_SQUARE_BRACKET;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> RIGHT_SQUARE_BRACKET;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> ADD;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> MINUS;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> MULTI;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> DIVIDE;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> LT;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> LE;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> GT;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> GE;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> EQ;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> EQEQ;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> EQEQEQ;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> OR;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> AND;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> OROR;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> ANDAND;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> WILDCARD_OP;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> DOT;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> COMMA;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> COLON;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> SEMICOLON;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> SIGN;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> HEKJE;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> QUESTION;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> EXCLAM;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> PATTERN_INSERTION;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> VARNAME;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> NUMBER;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> DECIMAL;
::deamer::type::SafeReserve<::deamer::language::type::definition::object::main::Terminal> ESCAPE_CHARS;

	
	public:
		Lexicon(dupr::Language* language)
			:	::deamer::language::generator::definition::property::user::Lexicon<
					::dupr::Language>(language)
		{
		}
		void GenerateObjects() override
		{
			// Terminals
			LEFT_BRACKET.Set(deamer::language::type::definition::object::main::Terminal("LEFT_BRACKET", "[{]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
RIGHT_BRACKET.Set(deamer::language::type::definition::object::main::Terminal("RIGHT_BRACKET", "[}]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
LEFT_PARANTHESIS.Set(deamer::language::type::definition::object::main::Terminal("LEFT_PARANTHESIS", "[(]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
RIGHT_PARANTHESIS.Set(deamer::language::type::definition::object::main::Terminal("RIGHT_PARANTHESIS", "[)]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
LEFT_SQUARE_BRACKET.Set(deamer::language::type::definition::object::main::Terminal("LEFT_SQUARE_BRACKET", "[\\[]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
RIGHT_SQUARE_BRACKET.Set(deamer::language::type::definition::object::main::Terminal("RIGHT_SQUARE_BRACKET", "[\\]]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
ADD.Set(deamer::language::type::definition::object::main::Terminal("ADD", "[+]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
MINUS.Set(deamer::language::type::definition::object::main::Terminal("MINUS", "[-]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
MULTI.Set(deamer::language::type::definition::object::main::Terminal("MULTI", "[*]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
DIVIDE.Set(deamer::language::type::definition::object::main::Terminal("DIVIDE", "[/]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
LT.Set(deamer::language::type::definition::object::main::Terminal("LT", "[<]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
LE.Set(deamer::language::type::definition::object::main::Terminal("LE", "[<][=]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
GT.Set(deamer::language::type::definition::object::main::Terminal("GT", "[>]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
GE.Set(deamer::language::type::definition::object::main::Terminal("GE", "[>][=]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
EQ.Set(deamer::language::type::definition::object::main::Terminal("EQ", "[=]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
EQEQ.Set(deamer::language::type::definition::object::main::Terminal("EQEQ", "[=][=]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
EQEQEQ.Set(deamer::language::type::definition::object::main::Terminal("EQEQEQ", "[=][=][=]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
OR.Set(deamer::language::type::definition::object::main::Terminal("OR", "[|]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
AND.Set(deamer::language::type::definition::object::main::Terminal("AND", "[&]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
OROR.Set(deamer::language::type::definition::object::main::Terminal("OROR", "[|][|]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
ANDAND.Set(deamer::language::type::definition::object::main::Terminal("ANDAND", "[&][&]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
WILDCARD_OP.Set(deamer::language::type::definition::object::main::Terminal("WILDCARD_OP", "([+]|[-]|[*]|[/]|[<]|[=]|[>]|[|]|[&])+", ::deamer::language::type::definition::object::main::SpecialType::Standard));
DOT.Set(deamer::language::type::definition::object::main::Terminal("DOT", "[.]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
COMMA.Set(deamer::language::type::definition::object::main::Terminal("COMMA", "[,]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
COLON.Set(deamer::language::type::definition::object::main::Terminal("COLON", "[:]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
SEMICOLON.Set(deamer::language::type::definition::object::main::Terminal("SEMICOLON", "[;]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
SIGN.Set(deamer::language::type::definition::object::main::Terminal("SIGN", "[$]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
HEKJE.Set(deamer::language::type::definition::object::main::Terminal("HEKJE", "[#]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
QUESTION.Set(deamer::language::type::definition::object::main::Terminal("QUESTION", "[?]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
EXCLAM.Set(deamer::language::type::definition::object::main::Terminal("EXCLAM", "[!]", ::deamer::language::type::definition::object::main::SpecialType::Standard));
PATTERN_INSERTION.Set(deamer::language::type::definition::object::main::Terminal("PATTERN_INSERTION", "([\\[][\\[][a-zA-Z_]+[a-zA-Z_0-9]*[\\]][\\]])|([\\[][\\[][a-zA-Z_>]+[a-zA-Z_0-9>]*[:][a-zA-Z_]+[a-zA-Z_0-9]*[\\]][\\]])", ::deamer::language::type::definition::object::main::SpecialType::Standard));
VARNAME.Set(deamer::language::type::definition::object::main::Terminal("VARNAME", "[a-zA-Z_]+[a-zA-Z_0-9]*", ::deamer::language::type::definition::object::main::SpecialType::Standard));
NUMBER.Set(deamer::language::type::definition::object::main::Terminal("NUMBER", "[0-9]+", ::deamer::language::type::definition::object::main::SpecialType::Standard));
DECIMAL.Set(deamer::language::type::definition::object::main::Terminal("DECIMAL", "[0-9]+[.][0-9]+", ::deamer::language::type::definition::object::main::SpecialType::Standard));
ESCAPE_CHARS.Set(deamer::language::type::definition::object::main::Terminal("ESCAPE_CHARS", "[\\n\\r\\t ]+", ::deamer::language::type::definition::object::main::SpecialType::Delete));

			// Add object calls
			// AddObject(...)
			AddObject(LEFT_BRACKET);
AddObject(RIGHT_BRACKET);
AddObject(LEFT_PARANTHESIS);
AddObject(RIGHT_PARANTHESIS);
AddObject(LEFT_SQUARE_BRACKET);
AddObject(RIGHT_SQUARE_BRACKET);
AddObject(ADD);
AddObject(MINUS);
AddObject(MULTI);
AddObject(DIVIDE);
AddObject(LT);
AddObject(LE);
AddObject(GT);
AddObject(GE);
AddObject(EQ);
AddObject(EQEQ);
AddObject(EQEQEQ);
AddObject(OR);
AddObject(AND);
AddObject(OROR);
AddObject(ANDAND);
AddObject(WILDCARD_OP);
AddObject(DOT);
AddObject(COMMA);
AddObject(COLON);
AddObject(SEMICOLON);
AddObject(SIGN);
AddObject(HEKJE);
AddObject(QUESTION);
AddObject(EXCLAM);
AddObject(PATTERN_INSERTION);
AddObject(VARNAME);
AddObject(NUMBER);
AddObject(DECIMAL);
AddObject(ESCAPE_CHARS);

			// Place higher level operations here.
			// ReplaceObject(..., ...)
			// DeleteObject(..., ...)
			
		}
	};
}
#endif // DUPR_LEXICON_H
