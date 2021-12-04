# DLJ2: DUPR (Deamer User Pattern Recognizer)

DLJ2 or alias for DUPR is a language solely oriented around patterns. The user needs to define patterns before the user can even do anything.

## Installation instructions

```bash
sudo python3 ./installer.py
```

This will install DeamerProject, this is required for compiling the compiler.

## Overview

We always have the same discussions about what pattern is the best:

- Should everything use trailing type? Or is inline good enough?
- Should we avoid using any types, and let the compiler deduce it? Or should we declare it ourselves?
- Are S-expressions the best, or should we all go to C like syntax.
- OOP is better than Functional,,,, or not?

In the end, we have sooooo many preferences for too many patterns.

DUPR says f*k this sh\*t idc anymore, let the user decide the patterns! We will then check if we can understand their patterns, if we cannot understand it, your ideal pattern is obviously flawed \s.

## Examples

### How to create your first pattern!

We always start with some entry function, type this in DUPR to see how one can create a function:

```
int main()
{
	return 0;
}
```

Oh no, DUPR says: "`Wtf are u doing?! Create your favorite pattern first and use that, you little piece of-`"

DUPR clearly doesn't like us anymore, so lets start creating some patterns, luckily DUPR has A Lot Of Ways to create Patterns. (We love you DUPR).

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
	int main()
	{
		return 0;
	}
}
```

And Voila, DUPR is finally happy, he can compile and run this!

But yet again, DUPR is still sad, we forgot the patterns for variables, classes, assignments, expressions, I can go on for ages.

Next part will explain how to make use of more patterns, because we love patterns.
