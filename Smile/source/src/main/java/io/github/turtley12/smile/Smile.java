package io.github.turtley12.smile;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.Map;

import io.github.turtley12.smile.parser.Parser;

public class Smile {
	private final static boolean example = false;
	private static String welcome = """
			Welcome to Smile!
			Smile is a language that uses emojis for almost everything.
			To run a smile file, run "smile yourprogram.smile"
			""";
	
	public static void main(String... args) throws IOException {
		//if only one arg, interpret
		if (args.length == 1) {
			interpret(args[0]);
		}
		else {
			System.out.println(welcome);
			if (example) {
				interpret(null);
			}
		}
		

	}
	private static void interpret(String path) throws IOException {
		File input_file;
		if (path != null) {
			input_file = new File(path);
			System.out.println("Running " + path);
		}
		else {
			System.out.println("Running example.smile");
			input_file = new File("example.smile");
		}
		new Parser().parse(Files.readAllLines(input_file.toPath()));
	}
	
}
