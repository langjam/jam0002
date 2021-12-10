#include "patty.hh"
#include <iostream>
#include <fstream>

namespace fs = std::filesystem;
using namespace std::string_view_literals;
using namespace fmt::literals;

fs::path program_name;
fs::path filename;

namespace version
{
	constexpr unsigned Major = 0;
	constexpr unsigned Minor = 1;
	constexpr unsigned Patch = 0;
	constexpr std::string_view Additional = "langjam0002";

	std::string string()
	{
		return fmt::format("{}.{}.{}-{}", Major, Minor, Patch, Additional);
	}
}

void usage()
{
	std::cout << "usage: " << program_name.c_str() << " [options] [filename]\n";
	std::cout << "  where \n";
	std::cout << "    filename is path to Patty program\n";
	std::cout << "      without filename REPL mode is launched\n\n";
	std::cout << "    options is one of:\n";
	std::cout << "      --doc       launch documentation in default browser (using xdg-open)\n";
	std::cout << "      --no-eval   don't evaluate\n";
	std::cout << "      --version   print version info\n";
	std::cout << "      -h,--help   print usage info\n";
	std::cout << std::flush;
	std::exit(1);
}

void help()
{
	std::cout << "To quit type :quit or press CTRL-D\n";
	std::cout << "Available commands:\n";
	std::cout << "  :global  Print variables defined in global namespace\n";
	std::cout << "  :help    Print this help message\n";
	std::cout << "  :quit    Quit this program\n";
	std::cout << "  :version Print version & author info\n";
}

void print_version(bool full = true)
{
	if (full) {
		std::cout << "Patty " << version::string() << '\n';
		std::cout << "Copyright (C) 2021 Robert Bendun <robert@bendun.cc>\n";
		std::cout << "This is free software; see the source for copying conditions. There is NO\n";
		std::cout << "warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE\n";
	} else {
		std::cout << "Patty " << version::string() << " by Robert Bendun <robert@bendun.cc>\n";
	}
}

void repl(Context &ctx)
{
	std::cout << " ____       _   _\n";
	std::cout << "|  _ \\ __ _| |_| |_ _   _\n";
	std::cout << "| |_) / _` | __| __| | | |\n";
	std::cout << "|  __/ (_| | |_| |_| |_| |\n";
	std::cout << "|_|   \\__,_|\\__|\\__|\\__, |\n";
	std::cout << "                    |___/\n\n";
	print_version(false);
	std::cout << "To get help type `help` or `:help`\n";

	while (std::cin) {
		std::string line;
		std::cout << "> " << std::flush;

		if (!std::getline(std::cin, line))
			break;
		std::string_view source = line;

		while (!source.empty() && std::isspace(source.front())) source.remove_prefix(1);
		while (!source.empty() && std::isspace(source.back())) source.remove_suffix(1);

		if (source == "help" || source == ":help") {
			help();
			continue;
		}

		if (source.starts_with(':')) {
			if (source == ":global") {
				for (auto &[name, value] : ctx.scopes.front()) {
					fmt::print("{}\t{}\n", name, value);
				}
				continue;
			}

			if (source == ":version") {
				print_version();
				continue;
			}

			if (source == ":quit") {
				std::exit(0);
			}

			std::cerr << "Unrecognized command\n";
			continue;
		}

		auto value = read(source);
		print(eval(ctx, std::move(value)));
	}
}

// TODO Parameter for printing result of evaluation
// TODO -c mode (like in Python or Bash)
int main(int, char **argv)
{
	program_name = fs::path(*argv++).filename();

	[[maybe_unused]] bool no_eval = false;

	for (; *argv != nullptr; ++argv) {
		if (*argv == "-h"sv || *argv == "--help"sv) {
			usage();
		}

		if (*argv == "--version"sv) {
			print_version();
			return 0;
		}

		if (*argv == "--doc"sv) {
			std::system("xdg-open 'https://github.com/RobertBendun/patty'");
			return 0;
		}

		if (*argv == "--no-eval"sv) { no_eval = true; continue; }

		if (filename.empty()) {
			filename = *argv;
		} else {
			error_fatal("more then one filename was specified");
		}
	}

	Context ctx;
	intrinsics(ctx);

	if (filename.empty()) {
		repl(ctx);
		return 0;
	}

	std::ifstream source_file(filename);
	if (!source_file) {
		error_fatal("cannot open file '{}'"_format(filename.c_str()));
	}

	std::string code(std::istreambuf_iterator<char>(source_file), {});

	std::string_view source = code;
	auto value = read(source);

	if (!no_eval) {
		(void)eval(ctx, std::move(value));
	} else {
		print(value);
	}
}
