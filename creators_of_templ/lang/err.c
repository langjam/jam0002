// Error printing

#include "err.h"
#include <stdio.h>
#include <stdarg.h>

// Creates a formatted error
Err err_f(ErrCode code, Location at, char *fmt, ...) {
	// va list setup
	va_list list;
	va_start(list, fmt);
	
	// create error at the location
	Err new_err = { .code = code, .location = at };
	
	// format the message 
	vsnprintf(new_err.buffer, sizeof(new_err.buffer), fmt, list);
	
	// release va list
	va_end(list);
	
	return new_err;
}

// Returns start of a specific line
char* linestart(char *source_file, int line) {
	while (line > 0 && *source_file) {
		if (*source_file++ == '\n') {
			line -= 1;
		}
		
	}
	
	return source_file;
}

// Prints a line from source
void print_line(char *source, int line) {
	// Retrieve the bounds of the line
	char *start = linestart(source, line);
	char *end = linestart(source, line+1);
	int len = (int)(end-start);
	int  ellipsis = 0;
	if (len > 50) {
		len = 50;
		ellipsis = 1;
	}

	// Print it
	printf("%.*s", len, start);
	if (ellipsis) {
		printf("...");
		printf("\n");
	}
}

// Prints formatted error into standard output
void err_explain(Err *err, char *source_file) {
	// We can't print success
	if (err->code == err_ok)
		return;
	// Store line for convenience
	int line = err->location.lineno;
	int col = err->location.charno;
	
	

	// Print message
	// TODO: We might want to print the note line as a parameter
	if (err->code == err_note) {
		printf("\x1b[34mNote:(%d,%d)\x1b[0m: %s\n", line + 1, col + 1, err->buffer);
	}
	else {
		printf("\x1b[31m[E%04d](%d,%d)\x1b[0m: %s\n", err->code, line + 1, col + 1, err->buffer);
	
		// Print line, add 1 to line number 
		// Because they are 1 indexed when printed
		if (line != 0) {
			printf("%4d | ", line	); print_line(source_file, line-1);
		}
		printf("%4d | ", line + 1); print_line(source_file, line);
		
		int ntab = 0;
		char *linestr = linestart(source_file, line);
		if (err->code == err_eof) {
			printf("\n");
		}
		printf("     | ");
		for (int i = 0; i < col; i += 1) {
			if (linestr[i] == '\t') {
				printf("\t");
				ntab += 1;
			}
		}
		printf(" %*c\n", err->location.charno-ntab, '^');
	}
}

