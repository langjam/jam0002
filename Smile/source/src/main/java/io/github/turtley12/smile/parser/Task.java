package io.github.turtley12.smile.parser;

public class Task {
	private final String name;
	private final int args;
	public Task(String name, int args ) {
		this.name = name;
		this.args = args;
	}
	public String getName() {
		return name;
	}
	public int getArgs() {
		return args;
	}
}
