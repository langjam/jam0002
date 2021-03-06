#ifndef TEMPLE_H
#define TEMPLE_H

#ifdef _MSC_VER
#define __attribute__(x)
#endif

#include "err.h"
#include <string.h>
#include <stdbool.h>

typedef enum {
	tok_inval,
	tok_lparen,
	tok_rparen,
	tok_lbrace,
	tok_rbrace,
	tok_dot,
	tok_colon,
	tok_semicolon,
	tok_comma,
	tok_at,
	tok_var,
	tok_keyword,
	tok_numlit,
	tok_strlit,
	tok_ident,
	tok_operator,
	tok_eof,
	tok_hexlit,
	tok_sep
} TokenType;

typedef struct {
	Location loc;
	TokenType type;
	char *val;
	int len;
} Token;

typedef struct {
	char *buf; // null terminated
	Location loc;
	int cursor;
	Err err;
	int catch_sep;
} Lexer;

extern const char *type_lookup[];

Token lex_next(Lexer *l);

bool tok_eq(Token tok, const char *s);
bool toks_eq(Token a, Token b);
void print_tok(Token tok);

#endif

