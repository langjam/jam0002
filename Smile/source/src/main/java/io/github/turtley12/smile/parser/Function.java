package io.github.turtley12.smile.parser;

import java.util.ArrayList;
import java.util.List;

public class Function {
	private final List<String> lines;
	//private final String[] args;
	public Function(List<String> lines ) {//String... args) {
		this.lines = new ArrayList<>();
		this.lines.addAll(lines);
		//this.args = args;
		
	}
	public List<String> getLines() {
		return lines;
	}
//	public String[] getArgs() {
//		return args;
//	}
}
