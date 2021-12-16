## clock [(vote)](https://github.com/langjam/jam0002/pull/17)
### by ngc6302
#### submitted 10 of december of 2021 for langjam 2


### About
clock is a simple programming language. It is only composed of single-line statements
written in a sort-of diary format, thus the "pattern"s. A clock porgram is both a first-person narration of a day and an executable program. Every statement is preceded by a time of day,
and this serves for both beauty and functional purposes.
To move variables between stack frames, the user must copy them to their backpack, which is carried around every time they *go to* anywhere.
Similarly, to return a value from a function call, the user must save it in the backpack and take it out once back at the calling function.
Since each instruction "happens" at a unique time,
it is possible to jump around by changing the time of day, or by "taking a nap" and letting time pass. Of course, while the program is napping,
the instructions that would happen during the time it is napping do not execute.
To end execution, the program must go to sleep for the night.



### Build instructions
cd to the main directory, and:

    git clone https://github.com/ngc6302h/neo.git  
    mkdir build && cd build  
    cmake .. -DCMAKE_BUILD_TYPE=Release
    make

The binary will be called clock.

### Run instructions
clock code_file_path_here  
#### Example:
clock ../factorial

clock ../patterns


### Clock syntax
- Natively UTF-8
- Every line, except blank lines and function declarations, must start
with a time, for example, ``13:15``.  
- Respect whitespaces! **only** 1 whitespace (no tabs allowed) between every statement.
See the sample code examples provided for reference.  
- When **reading a time** from stdin, the following syntax must be used: HH:MM
- Statements that begin without a command will make the whole line be printed verbatim to stdout.
- String literals must appear "quoted". Escape sequences (for example, \\n) are not supported.
- The following commands are allowed:  
    - go to *function_name*
    - wonder if *variable_name* *comparison_predicate* *variable_name/immediate*
        - Comparison predicates:
          - is greater or equal to (equivalent to C >=)
          - is (equivalent to C ==)
        - If true, jump to instruction_pointer+1. Else, jump to instruction_pointer+2.
    - go to sleep
      - sleep is a build-in function destination that terminates execution.
    - realize it's *hour*
      - Jump to time
    - write "*variable_name*"
      - Create new variable in current stack frame
    - ``say "*string_literal*" *variable_name*
      - See sample files for more examples
    - return from *function_name*
      - returns to the calling function. *function_name* is currently ignored
    - open my backpack, read *variable_name*, and write the value in *variable_name*
      - Copy variable from backpack to the current stack frame
    - put *variable_name* in my backpack
      - Copy variable to backpack
    - I notice the number in *varible_name* is *change_predicate* *variable_name2* now
      - *variable_name* and *variable_name2* may refer to the same variable.
      - Depending on the predicate, *variable_name2* may be optional.
      - Change predicates that **do not** need second operand:
        - is slightly greater (equivalent to C ++)
        - is slightly smaller (equivalent to C --)
      - Change predicates that need second operand:
        - is exactly greater by (equivalent to C +=)
        - is exactly smaller by (equivalent to C -=)
        - is the number in
          - Complex predicate. Instead of *variable_name2*, the format becomes *variable_name2* *operation* the number in *variable_name3* `or` *variable_name2* *operation* *integer_literal*
            - Operators:
              - plus (equivalent to C +)
              - minus (equivalent to C -)
              - multiplied by (equivalent to C *)
              - divided by (equivalent to C /)
          - There is no direct assignment predicate or operator (var1 = var2). Instead, you must use a complex operation that results in identity, such as multiplying by 1 or adding 0 (var1 = var2 + 0). See sample code for examples.
    - ask myself "*string_literal*" and write the answer in *variable_name*
      - read from stdin. If reading a number, just enter numbers. If reading a string, enter anything up to 1024 characters. If reading a time, use the before-mentioned format HH:MM
    - it feels like time doesn't pass
      - No operation. Statement is printed to stdout.
    - nap for as many hours as it's written in *variable_name*
      - Skip time
      - *variable_name* must contain a time string in HH:MM format
      - The time read is added to the current time to change the time of the day.
    - Good heavens, just look at the time! It's *time_string_literal*
      - Jump to the instruction at *time_string_literal*
    - erase *variable_name*
      - clear the contents of *variable_name*.
- and the rule most important of all... **Have fun**!
