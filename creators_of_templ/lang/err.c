// Error printing

#include "err.h"
#include <stdio.h>
#include <stdarg.h>

// Creates a formatted error
Err err_f(Location at, char *fmt, ...) {
    // va list setup
    va_list list;
    va_start(list, fmt);
    
    // create error at the location
    Err new_err = { .location = at };
    
    // format the message 
    vsnprintf(new_err.buffer, sizeof(new_err.buffer), fmt, list);
    
    // release va list
    va_end(list);
    
    return new_err;
}

// Returns start of a specific line
char* linestart(char *source_file, int line) {
    while (line > 0 && *source_file) {
        if (*source_file++ == '\n')
            line -= 1;
    }
    
    return source_file;
}

// Prints formatted error into standard output
void err_explain(Err *err, char *source_file) {
    // Store line for convenience
    int line = err->location.lineno;
    int col = err->location.charno;

    // Print message
    printf("\x1b[31mError(%d,%d)\x1b[0m: %s\n", line + 1, col + 1, err->buffer);
    
    // Retrieve the bounds of the line
    char *start = linestart(source_file, line);
    char *end = linestart(source_file, line+1);
    
    // Print line, add 1 to line number 
    // Because they are 1 indexed when printed
    printf("%4d | %.*s", line + 1, (int)(end-start), start);
        
    // Draw the cursor
    printf("        %*c\n", err->location.charno, '^');
}

