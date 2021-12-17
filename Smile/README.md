# Smile [(vote)](https://github.com/langjam/jam0002/pull/8)
Smile is a language that uses patterns of emojis to do things. Some things to keep in mind when using it are:
- Smile has implicit type declaration, except it doesn't really matter since there are only integers.
- New lines are important, every command must be on it's own line, unless it is embeded within another command such as `🍉 🚚 🍋🥭 1 2`
- Function arguments are provided after the function name, with spaces in between all arguments.
- Multiline commands such as loops and defining functions must end with 🥦
- All functions are prefix including Math operators
- User defined functions don't take arguments.
- Language function calls that aren't part of a variable assignment will print the result.



## Table of contents
- [Introduction](#smile)
- [Building and running](#building-and-running)
- [Cheatsheet](#cheatsheet)


## Building and running
 - Make sure you have OpenJDK 16 and Maven installed. The maven version in the Ubuntu package manager might not work, if it doen't work try installing the latest version from https://maven.apache.org/
 - To build the interpreter, run `mvn clean install` in the same directory as 'pom
xml'.
- Inside the target directory is smile.jar. Run that file with `java -jar smile.jar insertprogramhere.smile`
- Try running the examples in the `examples` directory.

## Cheatsheet

| Emoji |  Name          |Description         |
|-------|----------------|--------------------|
|🍋🥭  |   Add          | Adds argument 1 to argument 2.|
|🥭🍋  |   Subtract     | Subtracts argument 2 from argument 1.|
|🍎🍏  |  Multiply       | Multiplys argument 1 with argument 2.|
|🍏🍎  | Divide          | Divides argument 1 by argument 2.|
| 🍉   | Define Variable | Defines a variable. argument 1 is variable name, argument 2 is value. For example: `🍉 🛹 0 ` would set 🛹 to zero. You can also include an operation like `🍉 🚚 🍋🥭 1 2` that would set 🚚 To 1+2, or 3.|
|  🗺  | Define Function| Defines a function. Argument 1 is the function name. All lines below function declaration until nearest 🥦 are part of the function.|
|  🙂  |  Print         | Prints all arguments to the console. It will parse variable names and can print regular text. such as `🙂 Skateboard is 🛹`. |
|  🍅  | If Equal       | If argument 1 = argument 2, run the function name in argument 3. If not equal, run argument 4.|
|  🍍  | If Greater Than| If argument 1 > argument 2, run the function name in argument 3. If not greater than, run argument 4.|
|  🌟  | Random         | Returns random number between argument 1 and argument 2 (Both inclusive). For example, `🌟 1 5` will return 1,2,3,4, or 5. |
|  ❄  | Loop           | Loops all lines below it until nearest 🥦. Loop will stop when argument 1 and argument 2 are equal.|
|  ⌨  | Input          | Returns user input. input prompt is preceded by `> `.|
|  💬  | Comment        | Ignores all arguments. Intended for comments.|
|  🥦  | End multiline  | Ends multiline commands such as function definition and loops.|
