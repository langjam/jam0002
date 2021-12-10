#include "lexer.h"
#include <ctype.h>
#include <stdio.h>


bool tok_eq(Token tok, const char *s) {
	return (int)strlen(s) == tok.len && strncmp(tok.val, s, tok.len) == 0;
}

bool toks_eq(Token a, Token b) {
	return b.len == a.len && strncmp(b.val, a.val, a.len) == 0;
}

static inline int is_skip_char(char c) {
	return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

static char peekc(Lexer *l) {
	return l->buf[l->cursor];
}

static char nextc(Lexer *l) {
	char c = l->buf[l->cursor++];
	l->loc.charno++;
	if (c == '\n') {
		l->loc.lineno++;
		l->loc.charno = 0;
	}
	return c;
}

// Moves by 1 but returns actual current character
static char movec(Lexer *l) {
	nextc(l);
	return peekc(l);
}


bool skip_comments(Lexer *l) {
	if (peekc(l) == '/' && l->buf[l->cursor+1] == '/') {
		int startl = l->loc.lineno;
		while (startl == l->loc.lineno) nextc(l);
		return true;
	}
	if (peekc(l) == '/' && l->buf[l->cursor+1] == '*') {
		int stage = 0; 
		while (true) {
			if (stage == 1 && peekc(l) == '/')
				stage = 2;
			else if (peekc(l) == '*')
				stage = 1;
			else 
				stage = 0;
			nextc(l);
			if (stage == 2)
				break;
		}
		return true;
	}
	return false;
}

Token lex_next(Lexer *l) {

	while (true) {
		bool set = false;
		if (!l->catch_sep)
			for (char c=peekc(l); (set = is_skip_char(c) && c); c=peekc(l))
				nextc(l);
		set = skip_comments(l) || set;
		if (!set) break;
	}
	
	Token tok = { .loc = l->loc, .val = l->buf + l->cursor, .len = 1 };

	int move = 1;
	switch (peekc(l)) {
	case 0:
		tok.type = tok_eof;
		move = 0;
		break;
	case ' ':
	case '\t':
	case '\n':
	case '\r':
		tok.type = tok_sep;
		break;
	case '(':
		tok.type = tok_lparen;
		break;
	case ')':
		tok.type = tok_rparen;
		break;
	case '{':
		tok.type = tok_lbrace;
		break;
	case '}':
		tok.type = tok_rbrace;
		break;
	case '.':
		tok.type = tok_dot;
		break;
	case ',':
		tok.type = tok_comma;
		break;
	case ':':
		tok.type = tok_colon;
		break;
	case ';':
		tok.type = tok_semicolon;
		break;
	case '@':
		tok.type = tok_at;
		break;
	case '$':
		tok.type = tok_var;
		break;
	case '"':
		tok.type = tok_strlit;
		++tok.val;
		nextc(l);
		for (char c=nextc(l); c && c != '"'; c=peekc(l)) {
			tok.len++;
			nextc(l);
		}
		break;
	case '/': case '+': case '-': case '*': case '^': case '=':
		tok.type = tok_operator;
		break;
	case '#':
		tok.type = tok_hexlit;
		nextc(l);
		move = 0;
		for (char c = peekc(l); isdigit(c) ||
			(tolower(c) >= 'a' && tolower(c) <= 'f'); c = movec(l)) {
			++tok.len;
		}
		
		// Since we include the # symbol we dont want to account for it
		int nums = tok.len-1;

		if (nums != 6 && nums != 8) {
			tok.type = tok_inval;
			l->err = err_f(err_bad_number_literal, tok.loc, "Hex literal can't be %d long.", nums);
		}
		if (nums == 0) {
			l->err = err_f(err_bad_number_literal, tok.loc, "You can't have that character in hex color literal.");
		}
		break;
	default:;
		char c = peekc(l);
		int start_pos = l->cursor;
		tok.len = 0;
		
		if (isdigit(c) || c == '-') {
			tok.type = tok_numlit;
			int numdot = 0;
			for (c = peekc(l); isdigit(c) || c == '.'; c = movec(l)) {
				if (c == '.') numdot++;
				tok.len++;
			}
			
			// uh-oh -- too many dots in your number !
			if (numdot > 1) {
				if (numdot > 1000000) {
					l->err = err_f(err_bad_number_literal, tok.loc, "You put %d too many dots into your number! ARE YOU INSANE?!?!", numdot - 1);
				}
				else if (numdot > 9000) {
					l->err = err_f(err_bad_number_literal, tok.loc, "You put %d too many dots into your number! ITS OVER 9000!!", numdot - 1);
				}
				else if (numdot > 5) {
					l->err = err_f(err_bad_number_literal, tok.loc, "You put %d too many dots into your number! What the heck are you doing?", numdot - 1);
				}
				else if (numdot == 2) {
					l->err = err_f(err_bad_number_literal, tok.loc, "You put %d too many dots into your number! There are no version literals.", numdot - 1);
				}
				else {
					l->err = err_f(err_bad_number_literal, tok.loc, "You put %d too many dots into your number!", numdot - 1);
				}
				tok.type = tok_inval;
			}
		} else {
			tok.type = tok_ident;
			for (c = peekc(l); isalnum(c) || c == '_'; c = movec(l)) {
				tok.len++;
			}
		}
		
		// no valid token found !
		if (start_pos == l->cursor) {
			l->err = err_f(err_invalid_char, tok.loc, "I don't know this character!");
			tok.type = tok_inval;
		}

		move = 0;
	}

	if (move) nextc(l);

	return tok;
}

const char *type_lookup[] = {
	"invalid", "left paren", "right paren", "left brace", "right brace", "dot", "colon", "semicolon",
	"comma", "at", "variable", "keyword", "number", "string", "identifier",
	"operator", "eof", "hex literal", "separator"
};

void print_tok(Token tok) {
	printf("tok (%d: %d) \'%.*s\' %s\n",
		tok.loc.lineno, tok.loc.charno, tok.len, tok.val, type_lookup[tok.type]);
}

