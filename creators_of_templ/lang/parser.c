#include "parser.h"
#include <stdio.h>
#include <stdbool.h>

#define checkout(x) do { ErrCode err; if((err = (x))) { fprintf(stderr, ">>> %d %s:%d\n", err, __FILE__, __LINE__); return err;} } while (0)

// Creates a new parser from source code
Parser parser_init(char *src) {
	return (Parser) {
		.lexer.buf = src,
		.ast = ast_init()
	};
}

// Creates a snapshot of the parser
// some syntaxes require backtracking!
static Parser record(Parser *p) {
	return *p;
}

// Finds if token is problematic and returns non-zero if so
static ErrCode token_err(Parser *p, Token *tok) {
	// Lexer got wrong token!
	if (tok->type == tok_inval) {
		p->err = p->lexer.err;
		return p->lexer.err.code;
	}
	
	// End of file, handle it here
	if (tok->type == tok_eof) {
		p->err = err_f(err_eof, tok->loc, "Your file can't just end here!");
	}
	return 0;
}

// Peeks current token
static ErrCode peek_tok(Parser *p, Token *dest) {
	*dest = p->current;
	return token_err(p, &p->current);
}

// Returns if token matches
static bool is_tok(Parser *p, TokenType type) {
	return p->current.type == type;
}

// Returns if token doesn't match
static bool isnt_tok(Parser *p, TokenType type) {
	return p->current.type != type;
}

// Returns current token and puts next one down the line
static ErrCode next_tok(Parser *p, Token *dest) {

	
	// You can pass NULL and it will just skip
	if (dest == NULL) {
		// Put next token
		p->current = lex_next(&p->lexer);
		return err_ok;
	}
	else {
		// Peek buffered token
		checkout(peek_tok(p, dest));
	}

	// Put next token
	p->current = lex_next(&p->lexer);
	
	// Everything is good
	return err_ok;
}

// Returns token if their types match
static ErrCode give_tok(Parser *p, TokenType type, Token *dest) {
	// Check if token is valid
	if (isnt_tok(p, type)) {
		p->err = err_f(err_unexpected, p->current.loc, "I didn't expect that. (I wanted `%s' but this is `%s')",
														type_lookup[type], type_lookup[p->current.type]);
		return err_unexpected;
	}

	// Put next token
	checkout(next_tok(p, dest));
	
	// Everything is good
	return err_ok;
}

// Parses a simple non-recursive selector
static ErrCode simple_selector(Parser *p, Node *dest) {
	Token name;
	checkout(give_tok(p, tok_ident, &name));

	// Set the node 
	node_set(dest, node_from(node_simple_selector, name));	
	return err_ok;
}


static ErrCode property_list(Parser *p, Node *dest);
static ErrCode selector_and_props(Parser *p, Node *dest) {
	node_set(dest, node_of(node_selector_and_props));
	checkout(simple_selector(p, ast_make(&p->ast, dest)));
	return property_list(p, ast_make(&p->ast, dest));
}

static ErrCode atom(Parser *p, Node *dest);

static ErrCode call(Parser *p, Node *dest) {
	checkout(give_tok(p, tok_lparen, NULL));
	
	while (isnt_tok(p, tok_rparen)) {
		// Parse other atoms
		checkout(atom(p, ast_make(&p->ast, dest)));
		
		// If rparen is met no don't expect the last comma
		if (isnt_tok(p, tok_rparen))
			checkout(give_tok(p, tok_comma, NULL));
	}
	
	return give_tok(p, tok_rparen, NULL);
}

static ErrCode atom(Parser *p, Node *dest) {
	Token current; checkout(next_tok(p, &current));
	
	switch (current.type) {
		case tok_numlit:
		case tok_ident:
		case tok_strlit:			
			// This is a call
			if (current.type == tok_ident && is_tok(p, tok_lparen)) {
				checkout(call(p, dest));
			}
			// This isn't a call
			else {
				node_set(dest, node_from(node_atom, current));
			}
		break;
		default:
			p->err = err_f(err_unexpected, current.loc, "I don't know what does %s do here", type_lookup[current.type]);
			return err_unexpected;
	}
	return err_ok;
}


static ErrCode property(Parser *p, Node *dest) {
	// Record savestate, because it can either be a property or a child! 
	Parser savestate = record(p);
	Token name;
	checkout(give_tok(p, tok_ident, &name));
	if (is_tok(p, tok_colon)) {
		// Skip colon
		checkout(next_tok(p, NULL));
		node_set(dest, node_of(node_property));
		
		// TODO: Parsing multiple properties
		while (isnt_tok(p, tok_semicolon)) {
			checkout(atom(p, ast_make(&p->ast, dest)));
		}
		checkout(give_tok(p, tok_semicolon, NULL)); 
	}
	else {
		// Restore savestate
		*p = savestate;
		checkout(selector_and_props(p, dest));
	}
	return err_ok;
}


static ErrCode property_list(Parser *p, Node *dest) {
	checkout(give_tok(p, tok_lbrace, NULL));
	node_set(dest, node_of(node_property_list));
	
	// Until we meet the closing brace
	while (isnt_tok(p, tok_rbrace)) {
		checkout(property(p, ast_make(&p->ast, dest)));
	}
	
	// Close the brace
	return give_tok(p, tok_rbrace, NULL);
}

// Runs the parser, and returns non-zero error code if something happened
// Check parser->err to find the details of the error
ErrCode parser_run(Parser *p) {
	Node *root = ast_make(&p->ast, NULL);
	p->current = lex_next(&p->lexer);
	return selector_and_props(p, root); 
}

// Frees data from parser
void parser_deinit(Parser *p) {
	ast_deinit(&p->ast);
}

