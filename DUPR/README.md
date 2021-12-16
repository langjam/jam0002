# DLJ2: DUPR (Deamer User Pattern Recognizer) [(vote)](https://github.com/langjam/jam0002/pull/22)

DLJ2 or alias for DUPR is a language solely oriented around patterns. The user needs to define patterns before the user can even do anything.

## Installation instructions

```bash
sudo python3 ./installer.py
```

This will clone and install DeamerProject (https://github.com/Deruago/theDeamerProject), this is required for compiling the compiler. If you do not trust the installer, you can always manually clone the project and install it manually using cmake, follow the instruction by following the earlier link.

Prerequisites:

- Git
- Python3 (if you want to use the installer.py)
- Cmake
- Compatible C++17 compiler
- Use some linux distro, or when on windows use WSL.

## Overview

We always have the same discussions about what pattern is the best:

- Should everything use trailing type? Or is inline good enough?
- Should we avoid using any types, and let the compiler deduce it? Or should we declare it ourselves?
- Are S-expressions the best, or should we all go to C like syntax.
- OOP is better than Functional,,,, or not?

In the end, we have sooooo many preferences for too many patterns.

DUPR says f*k this sh\*t idc anymore, let the user decide the patterns! We will then check if we can understand their patterns, if we cannot understand it, your ideal pattern is obviously flawed \s.

## Content

- Overview
- Content
- How To Compile DUPR?
- Examples
- How DUPR works
- Definable Patterns

## How to compile DUPR?

### Install Deamer Project

make sure you have started the installer located at: "/DUPR/installer.py".

```
cd ./DUPR
sudo python3 ./installer.py
```

The above script will install Deamer Project (https://github.com/Deruago/theDeamerProject), after the clone, manual confirmation is required to install Deamer Project.

DUPR requires the latest Deamer version

### Compile DUPR

From the langjam root directory run the following command:

```
cd ./DUPR/Project/dupr
```

Now you are in the right directory, we want to create a build directory and compile the project. Make sure Deamer Project is installed.

```
mkdir build
cd build
cmake ..
cmake --build . --target dupr_main
```

The build directory is where you write your code. The code needs to be in the input.dupr file, located in the build directory (create the file).

- /build/dupr_main
- /build/input.dupr

## Examples

### How do you compile code?

Create the file  "input.dupr", this is where you input your code.

Then do this to compile the code:

```
./dupr_main && g++ dupr_main.cpp -o dupr_output && ./dupr_output
```

If everything went fine, dupr_output will print the return value of the entry function. (Had not enough time to properly implement print functionality)

Make sure you run this in the build directory with the executable and input.dupr file.

### How to create your first pattern!

We always start with some entry function, type this in DUPR to see how one can create a function:

```
int main()
{
	return 0;
}
```

Oh no, DUPR says: "`Wtf are you doing?! (syntax error), make sure you create some pattern, and use this pattern!`"

DUPR clearly doesn't like us anymore, so lets start creating some patterns, luckily DUPR has A Lot Of Ways to create Patterns.

So Lets create a function pattern.

```C++
FunctionPattern: TheBestFunctionPattern {
    [[return_type]] [[name]]([[arguments]])
    {
        [[statements]]
    }
}
```

We now can use the above pattern to implement our main function:

```
FunctionPattern: TheBestFunctionPattern {
    [[return_type]] [[name]]([[arguments]])
    {
        [[statements]]
    }
}

TheBestFunctionPattern {
	int main()
	{
		return 0;
	}
}
```

Wait, I tried compiling this and it doesn't work?

Yes, we missed a lot of patterns. How does DUPR know how you want to format your arguments? Or how your return statement is structured? Of course DUPR does not know this!!!! So lets supply DUPR with those patterns too.

```
ArgumentPattern: ThePeopleThatDoNotUseThisPatternForArgumentsAreStupid [
	ArgumentContentPattern: Argument {
		[[type]] [[name]]
	}
	ArgumentExtensionPattern: Extension {
		,
	}
]

StatementPattern: ILovePeopleWhoUseThisPatternForStatements [
	ReturnPattern: Return {
		return [[expression]];
	}
]

FunctionPattern: TheBestFunctionPattern {
    [[return_type]] [[name]]([[ThePeopleThatDoNotUseThisPatternForArgumentsAreStupid:arguments]])
    {
        [[ILovePeopleWhoUseThisPatternForStatements:statements]]
    }
}

TheBestFunctionPattern {
	int dupr_main()
	{
		return 10;
	}
}
```

And Voila, DUPR is finally happy, he can compile and run this! Notice that the entry function IS NOT "main" instead the entry function is: dupr_main.

But yet again, DUPR is still sad, we forgot the patterns for variables, classes, assignments, expressions, I can go on for ages.

Next part will explain how to make use of more patterns, because we love patterns. (I will just show some examples, hopefully you understand it enough to create your own patterns)

### C style

```
StatementPattern: CStatement [	
	ReturnPattern: Return {
		return [[expression]];
	}
	
	VariableDeclarationPattern: VariableDeclaration {
		[[type]] [[name]];
	}
	
	VariableInitializationPattern: VariableInitialization {
		[[type]] [[name]] = [[expression]];
	}

	VariableAssignmentPattern: VariableAssignment {
		[[name]] = [[expression]];
	}
	
	ConditionalPattern: Conditional [
		IfPattern: If {
			if ([[expression]])
			{
				[[CStatement:statements]]
			}
		}
		
		ElseIfPattern: ElseIf {
			else if ([[expression]])
			{
				[[CStatement:statements]]
			}
		}
		
		ElsePattern: Else {
			else
			{
				[[CStatement:statements]]
			}
		}
	]
]

ArgumentPattern: CArguments [
	ArgumentContentPattern: Argument {
		[[type]] [[name]]
	}
	ArgumentExtensionPattern: Extension {
		,
	}
]

FunctionPattern: CFunction {
	[[return_type]] [[name]]([[CArguments:arguments]])
	{
		[[CStatement:statements]]
	}
}

CFunction {
	int fibonacci(int n)
	{
		if (n <= 0)
		{
			return 0;
		}
	
		if (n == 1)
		{
			return 1;
		}
		
		return fibonacci(n - 1) + fibonacci(n - 2);
	}
}

CFunction {
	int dupr_main()
	{
		return fibonacci(10);
	}
}
```

Running the above code, results in the following to be printed:

```
55
```

You can change the number 10 to different numbers, and you will see it indeed correctly executes fibonacci.

### S expression style

```
StatementPattern: SexprStatement [
	ReturnPattern: Return {
		(return [[expression]])
	}
	
	VariableDeclarationPattern: VariableDeclaration {
		([[type]] [[name]])
	}
	
	VariableInitializationPattern: VariableInitialization {
		([[type]] [[name]] = [[expression]])
	}

	VariableAssignmentPattern: VariableAssignment {
		([[name]] = [[expression]])
	}
	
	ConditionalPattern: Conditional [
		IfPattern: If {
			(if [ [[expression]] ]
				[[SexprStatement:statements]]
			)
		}
		
		ElseIfPattern: ElseIf {
			(elseif [ [[expression]] ]
				[[SexprStatement:statements]]
			)
		}
		
		ElsePattern: Else {
			(else
				[[SexprStatement:statements]]
			)
		}
	]
]

ArgumentPattern: SexprArguments [
	ArgumentContentPattern: Argument {
		([[type]]: [[name]])
	}
	ArgumentExtensionPattern: Extension {
		
	}
]

FunctionPattern: SexprFunction {
	(function
		(returntype [[return_type]] 
			(name [[name]]
				(Args: [[SexprArguments:arguments]])
				([[SexprStatement:statements]])
			)
		)
	)
}

SexprFunction {
	(function
		(returntype int
			(name fibonacciSexpr
				(Args: (int: n)
				)
				(
					(if [n <= 0] (return 0))
					(elseif [n == 1] (return 1))
					(else (return fibonacciSexpr(n - 1) + fibonacciSexpr(n - 2)))
				)
			)
		)
	)
}


SexprFunction {
	(function
		(returntype int
			(name dupr_main
				(Args: (unused: _))
				(
					(return fibonacciSexpr(10))
				)
			)
		)
	)
}
```

As you will see, in this pattern we cannot have empty arguments, in this case we use the type "unused", this tells DUPR to ignore the argument.

Running the above code, results in the following to be printed:

```
55
```

You can change the number 10 to different numbers, and you will see it indeed correctly executes fibonacci.

### Combining patterns

You can obviously also choose to combine patterns, as you will see in the next example, embedded statements inside conditionals change what pattern is required (c style -> s expr style -> c style):

```
StatementPattern: CStatement [	
	ReturnPattern: Return {
		return [[expression]];
	}
	
	VariableDeclarationPattern: VariableDeclaration {
		[[type]] [[name]];
	}
	
	VariableInitializationPattern: VariableInitialization {
		[[type]] [[name]] = [[expression]];
	}

	VariableAssignmentPattern: VariableAssignment {
		[[name]] = [[expression]];
	}
	
	ConditionalPattern: Conditional [
		IfPattern: If {
			if ([[expression]])
			{
				[[SexprStatement:statements]]
			}
		}
		
		ElseIfPattern: ElseIf {
			else if ([[expression]])
			{
				[[SexprStatement:statements]]
			}
		}
		
		ElsePattern: Else {
			else
			{
				[[SexprStatement:statements]]
			}
		}
	]
]

ArgumentPattern: CArguments [
	ArgumentContentPattern: Argument {
		[[type]] [[name]]
	}
	ArgumentExtensionPattern: Extension {
		,
	}
]

FunctionPattern: CFunction {
	[[return_type]] [[name]]([[CArguments:arguments]])
	{
		[[CStatement:statements]]
	}
}

StatementPattern: SexprStatement [
	ReturnPattern: Return {
		(return [[expression]])
	}
	
	VariableDeclarationPattern: VariableDeclaration {
		([[type]] [[name]])
	}
	
	VariableInitializationPattern: VariableInitialization {
		([[type]] [[name]] = [[expression]])
	}

	VariableAssignmentPattern: VariableAssignment {
		([[name]] = [[expression]])
	}
	
	ConditionalPattern: Conditional [
		IfPattern: If {
			(if [ [[expression]] ]
				[[CStatement:statements]]
			)
		}
		
		ElseIfPattern: ElseIf {
			(elseif [ [[expression]] ]
				[[CStatement:statements]]
			)
		}
		
		ElsePattern: Else {
			(else
				[[CStatement:statements]]
			)
		}
	]
]

ArgumentPattern: SexprArguments [
	ArgumentContentPattern: Argument {
		([[type]]: [[name]])
	}
	ArgumentExtensionPattern: Extension {
		
	}
]

FunctionPattern: SexprFunction {
	(function
		(returntype [[return_type]] 
			(name [[name]]
				(Args: [[SexprArguments:arguments]])
				([[SexprStatement:statements]])
			)
		)
	)
}

CFunction {
	int some_embedded_function(int n)
	{
		if (n <= 0)
		{
			(if [ n == -1 ] return 1234;)
			(else return n;)
		}
	
		if (n == 1)
		{
			(return 1)
		}
		
		return 2;
	}
}

CFunction {
	int dupr_main()
	{
		return some_embedded_function(-1);
	}
}
```

As you see we define in the CStatement->ConditionalIf that the underlying statements are SexprStatements. This directs DUPR to use the Sexprstatement pattern, by doing the same with Sexprstatement, we construct an alternating pattern. Requiring alternation between the 2 patterns.

### How does DUPR work?

#### Generic parsing

DUPR works by very generically parse the input, the only things DUPR extracts is encapsulation structures. Now we have the overal structure of our input, however there are no patterns which we can use to match this generic structure with some IR.

#### Pattern matching

DUPR has a lot of different patterns the user can implement. Each pattern maps a structural pattern with some IR translation. E.g. the function pattern supports the keywords:

- [[return_type]]
- [[name]]
- [[arguments]]
- [[statements]]

The above matchers, have semantic value. When matched DUPR knows how to translate the matched content with the IR.

#### Embedded patterns

As seen in the last example, one can embed patterns, this is done by specifying which pattern should be used to match something. Some keywords allow patterns to be used for their matching, e.g.:

- [[arguments]]
- [[statements]]

The user supplies the pattern as follows: [[UsedPattern:arguments]], this tells DUPR that matching the arguments, requires to use the "UsedPattern" pattern.

As some patterns such as conditional (embed [[statements]]), the user is able to define alternating patterns:

```
StatementPattern: CStatement [	
	ConditionalPattern: Conditional [
		ElsePattern: Else {
			else
			{
				[[SStatement:statements]]
			}
		}
	]
]

StatementPattern: SStatement [	
	ConditionalPattern: Conditional [
		ElsePattern: Else {
			else
			[
				[[CStatement:statements]]
			]
		}
	]
]
```

#### Multiple patterns

DUPR supports multiple patterns to be used to match something, this allows the user to support multiple patterns using the same matcher. This is done by defining multiple patterns with the same name, using a simple precedence check we take the first pattern in case of ambiguity.

#### Why can I not specify expression keyword?

Simple, I had not enough time to do this. So for now, it maps with C like code, if you stick with basic operations and function calls as you would do in c, expressions should properly match.

## Definable patterns

The examples show how one can construct code via patterns, and it shows that you can embed patterns inside patterns.

However, which patterns can we implement?

### Function

Pattern: FunctionPattern

Name: You may choose for yourself

```
FunctionPattern: CFunction {
	[[return_type]] [[name]]([[CArguments:arguments]])
	{
		[[CStatement:statements]]
	}
}
```

| Required mappings: | Usage                                       | Match type   |
| ------------------ | ------------------------------------------- | ------------ |
| return_type        | Maps with a return type in the IR           | Direct match |
| name               | Maps with a function name in the IR         | Direct match |
| arguments          | Maps with a sequence of arguments in the IR | Pattern      |
| statements         | Map with a sequence of statements in the IR | Pattern      |

### Arguments

Pattern: ArgumentPattern

Name: You may choose for yourself

#### Argument

Pattern: ArgumentContentPattern

Name: Argument

```
ArgumentPattern: SexprArguments [
	ArgumentContentPattern: Argument {
		([[type]]: [[name]])
	}
]
```

| Required mappings: | Usage                               | Match type   |
| ------------------ | ----------------------------------- | ------------ |
| type               | Maps with a argument type in the IR | Direct match |
| name               | Maps with a argument name in the IR | Direct match |

#### Extension

Pattern: ArgumentExtensionPattern

Name: Extension

```
ArgumentPattern: CArguments [
	ArgumentExtensionPattern: Extension {
		,
	}
]
```

The sequence exists of a predefined list of characters.

### Statement

Pattern: StatementPattern

Name: You may choose for yourself

#### Return statement

Pattern: ReturnPattern

Name: Return

```
StatementPattern: CStatement [	
	ReturnPattern: Return {
		return [[expression]];
	}
]
```

| Required mappings: | Usage                             | Match type         |
| ------------------ | --------------------------------- | ------------------ |
| expression         | Maps with an expression in the IR | Predefined pattern |

#### Variable Declaration

Pattern: VariableDeclarationPattern

Name: VariableDeclaration

```
StatementPattern: CStatement [	
	VariableDeclarationPattern: VariableDeclaration {
		[[type]] [[name]];
	}
]
```

| Required mappings: | Usage                               | Match type   |
| ------------------ | ----------------------------------- | ------------ |
| type               | Maps with a variable type in the IR | Direct match |
| name               | Maps with a variable name in the IR | Direct match |

#### Variable Assignment

Pattern: VariableAssignmentPattern

Name: VariableAssignment

```
StatementPattern: CStatement [	
	VariableAssignmentPattern: VariableAssignment {
		[[name]] = [[expression]];
	}
]
```

| Required mappings: | Usage                               | Match type         |
| ------------------ | ----------------------------------- | ------------------ |
| name               | Maps with a variable name in the IR | Direct match       |
| expression         | Maps with an expression in the IR   | Predefined pattern |



#### Variable Initialization

Pattern: VariableInitializationPattern

Name: VariableInitialization

```
StatementPattern: CStatement [	
	VariableInitializationPattern: VariableInitialization {
		[[type]] [[name]] = [[expression]];
	}
]
```

| Required mappings: | Usage                               | Match type         |
| ------------------ | ----------------------------------- | ------------------ |
| type               | Maps with a variable type in the IR | Direct match       |
| name               | Maps with a variable name in the IR | Direct match       |
| expression         | Maps with an expression in the IR   | Predefined pattern |



#### Conditional If

Pattern: IfPattern

Name: If

```
StatementPattern: CStatement [	
	ConditionalPattern: Conditional [
		IfPattern: If {
			if ([[expression]])
			{
				[[CStatement:statements]]
			}
		}
	]
]
```

| Required mappings: | Usage                                                        | Match type         |
| ------------------ | ------------------------------------------------------------ | ------------------ |
| statements         | Maps with statements executed if expression is true in the IR | Pattern            |
| expression         | Maps with an boolean expression in the IR                    | Predefined pattern |



#### Conditional Else If

Pattern: ElseIfPattern

Name: ElseIf

```
StatementPattern: CStatement [	
	ConditionalPattern: Conditional [
		ElseIfPattern: ElseIf {
			else if ([[expression]])
			{
				[[CStatement:statements]]
			}
		}
	]
]
```

| Required mappings: | Usage                                                        | Match type         |
| ------------------ | ------------------------------------------------------------ | ------------------ |
| statements         | Maps with statements executed if expression is true in the IR | Pattern            |
| expression         | Maps with an boolean expression in the IR                    | Predefined pattern |



#### Conditional Else

Pattern: ElsePattern

Name: Else

```
StatementPattern: CStatement [	
	ConditionalPattern: Conditional [
		ElsePattern: Else {
			else
			{
				[[CStatement:statements]]
			}
		}
	]
]
```

| Required mappings: | Usage                                                        | Match type |
| ------------------ | ------------------------------------------------------------ | ---------- |
| statements         | Maps with statements executed if expression is true in the IR | Pattern    |

## Reflection

I still do not know how to categorize DUPR, is it a parser gen? or is it just a very lenient language?

Anyway, I liked writing DUPR, it was a completely different way of writing my ordinary languages, and I love how DUPR allows me to easily test trivial languages.

There are still a lot of features left to implement, to make it perfect. So I will keep it along my other side-projects.

Make sure to check out Deamer Project: (https://github.com/Deruago/theDeamerProject)

