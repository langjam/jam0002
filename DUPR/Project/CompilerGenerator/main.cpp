#include "dupr/CompilerGenerator.h"

#include "Deamer/File/Generate/Compiler.h"

int main(int argc, char* argv[])
{
		// Generate: dupr
	::dupr::CompilerGenerator dupr_compiler;
	::deamer::file::generate::Compiler dupr_WriteToDisk(dupr_compiler.Generate());
	dupr_WriteToDisk.SetProjectType(::deamer::file::generate::Compiler::ProjectType::single);
	dupr_WriteToDisk.Generate();


	return 0;
}
