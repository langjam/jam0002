package io.github.turtley12.smile.parser;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import org.apache.commons.lang3.ArrayUtils;

public class Parser {
	private final Map<String, Object> variables = new HashMap<>();
	private final List<String> input = new ArrayList<>();
	private final List<Integer> blocked_indexes = new ArrayList<>();

	public void parse(List<String> input) {
		this.input.clear();
		this.input.addAll(input);
		try {
			loopThrough(input);
		} catch (TypeException | NumberFormatException | IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	private void loopThrough(List<String> input) throws TypeException, NumberFormatException, IOException {
		List<Object> out = new ArrayList<Object>();
		int index = 0;
		for (String in : input) {
			String[] command = in.split(" ");
			Object i = interpret(command, index);
			if (i != null) {
				System.out.println(i);
			}
			index++;
		}
		//return out.toArray();
	}

	public Object interpret(String[] command, int index) throws TypeException, NumberFormatException, IOException {

		// check for comments inline and ignore
		if (ArrayUtils.contains(command, "💬")) {
			int comment_index = ArrayUtils.indexOf(command, "💬");
			command = ArrayUtils.subarray(command, 0, comment_index);
		}
		if (command.length == 0) {
			return null;
		}
		if (blocked_indexes.contains(index)) {
			return null;
		}
		switch (command[0]) {

		// let
		case "🍉":
			if (command.length == 3) {
				addVariable(command[1], parseVariable(command[2]));
			}
			else if (command.length > 3 ) {
				// get rid of let command and name;
				List<String> list = Arrays.asList(command).subList(2, command.length);

				// interpret rest of it
				addVariable(command[1], interpret(list.toArray(new String[list.size()]), -1));
			}
			break;
		// define function. argument 1 is the variable name. function goes until
		// nearest🥦
		case "🗺":
			int end_index1 = indexOf(input, "🥦", index);
			// List<String> loop_lines = input.subList(index, end_index);
			List<String> sublist = input.subList(index + 1, end_index1);
			addVariable(command[1], new Function(sublist));
			// input.removeAll(sublist);

			// adding to blocked list to prevent it from being run.
			for (String s : sublist) {
				blocked_indexes.add(input.indexOf(s));
			}

			break;

		// lemon+mango add
		case "🍋🥭":
			return parseVariableAsInt(command[1]) + parseVariableAsInt(command[2]);
		// mango+lemon subtract
		case "🥭🍋":
			return parseVariableAsInt(command[1]) - parseVariableAsInt(command[2]);
		// red +green apple multiply
		case "🍎🍏":
			return parseVariableAsInt(command[1]) * parseVariableAsInt(command[2]);
		// green+red apple divide
		case "🍏🍎":
			return parseVariableAsInt(command[1]) / parseVariableAsInt(command[2]);

		// print
		case "🙂":
			for (int i = 1; i < command.length; i++) {
				System.out.print(parseVariable(command[i]) + " ");
			}
			System.out.println();
			break;

		// loop until arguments are equal to each other.
		case "❄":
			int end_index = indexOf(input, "🥦", index);
			// List<String> loop_lines = input.subList(index, end_index);
			while (parseVariableAsInt(command[1]) != parseVariableAsInt(command[2])) {
				loopThrough(input.subList(index + 1, end_index));
			}
			break;

		// equal if
		// if argument 1 and argument 2 are equal, run the function in argument 3. if
		// not runs the function in argument 4
		case "🍅":
			//System.out.println(ArrayUtils.toString(parseVariableArray(command)));
			if (parseVariable(command[1]).toString().equals(parseVariable(command[2]).toString())) {
				runFunction(command[3]);
			} else {
				if (command.length>4) {
					runFunction(command[4]);
				}
			}
		// greaterthan if
		// if argument 1 is greater than argument 2, run the function in argument 3. if
		// not runs the function in argument 4
		case "🍍":
			//System.out.println(ArrayUtils.toString(parseVariableArray(command)));
			if (parseVariableAsInt(command[1]) > (parseVariableAsInt(command[2]))) {
				runFunction(command[3]);
			} else {
				if (command.length>4) {
					runFunction(command[4]);
				}
				
			}
		// random between argument 1 (inclusive) and argument 2 (inclusive)
		case "🌟":
			Random r = new Random();
			int first_arg = parseVariableAsInt(command[1]);
			int random = r.nextInt(parseVariableAsInt(command[2]) - first_arg + 1);
			return random + (first_arg);
		// input
		case "⌨":
			System.out.print("> ");
			BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
			return Integer.parseInt(reader.readLine());

		case "💬":
			return null;

		// run functions
		default:
			runFunction(command[0]);
			break;

		}

		return null;
	}

	public Object parseVariable(String in) throws NumberFormatException, TypeException, IOException {
		if (in.equals("⌨")) {
			return interpret(new String[] {"⌨"}, -1);
		}
		try {
			return Integer.parseInt(in);
		} catch (Exception e) {
			if (variables.containsKey(in)) {
				return variables.get(in);
			}
		}
		return in;
	}

	public Integer parseVariableAsInt(String in) throws TypeException, NumberFormatException, IOException {
		//System.out.println("Parsing in: " + in);
		if (parseVariable(in) instanceof Integer) {
			return (Integer) parseVariable(in);
		}
		try {
			return Integer.parseInt((String) parseVariable(in));
		} catch (Exception e) {
			if (variables.containsKey(in)) {
				return (Integer) variables.get(in);
			}
			throw new TypeException(in + " is not an integer.");
		}
		//throw new TypeException(in + " is not an integer.");
	}

	private int indexOf(List<String> in, String search, int start) {
		for (int index = 0; index < in.size(); index++) {
			String line = in.get(index);
			if (line.startsWith(search)) {
				if (index > start) {
					return index;
				}
			}
		}
		return -1;
	}

	private void runFunction(String in) throws TypeException, NumberFormatException, IOException {
		if (variables.containsKey(in)) {
			Object value = variables.get(in);
			if (value instanceof Function) {
				loopThrough(((Function) value).getLines());
			}
		}
	}

	private String[] parseVariableArray(String[] in) throws NumberFormatException, TypeException, IOException {
		for (int i = 0; i < in.length; i++) {
			if (variables.containsKey(in[i])) {
				in[i] = parseVariable(in[i]).toString();
			}
		}
		return in;

	}
	private void printAll(Object[] objects) {
		for (Object o : objects) {
			System.out.println(o);
		}
	}
	private void addVariable(String name, Object value) {
//		System.out.println("Name: " + name);
//		System.out.println("Value: "+value);

		variables.put(name, value);
	}
}
