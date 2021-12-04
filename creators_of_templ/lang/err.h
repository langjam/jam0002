// Error printing
#ifndef ERR_H
#define ERR_H

enum ErrCode {
	err_note			   = -1,
	err_ok				 = 0,
	err_eof				= 1,
	err_invalid_char	   = 2,
	err_bad_number_literal = 3,
	// Parser errors
	err_unexpected		 = 4
} /*__attribute__((warn_unused_result))*/;

typedef enum ErrCode ErrCode;

typedef struct Location {
	int charno;
	int lineno;
} Location;

typedef struct Err {
	char buffer[4096];
	ErrCode code;
	Location location;
} Err; 

// Creates a formatted error
Err err_f(ErrCode code, Location at, char *fmt, ...);

// Prints formatted error into standard output
void err_explain(Err *err, char *source_file);

#endif // ERR_H

