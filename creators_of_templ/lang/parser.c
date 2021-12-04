#include "parser.h"
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#define checkout(x) do { ErrCode err; if((err = (x))) { fprintf(stderr, ">>> %d %s:%d\n", err, __FILE__, __LINE__); return err;} } while (0)
#define push_note(...) for(p->note = err_f(err_note, __VA_ARGS__);p->note.code; p->note = (Err) { 0 })

static ErrCode property_list(Parser *p, Node *dest);
static ErrCode atom(Parser *p, Node *dest);
static ErrCode value(Parser *p, Node *left, int prec);

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
	
	NodeType type = node_primitive_selector;
	
	// This is a class
	if (is_tok(p, tok_dot)) {
		checkout(next_tok(p, NULL));
		type = node_class_selector;
	}
	
	push_note(name.loc, "I was parsing a selector") {
		checkout(give_tok(p, tok_ident, &name));
	}

	// Set the node 
	node_set(dest, node_from(type, name));	
	return err_ok;
}

static ErrCode composite_selector(Parser *p, Node *dest) {
	node_set(dest, node_of(node_composite_selector));
	while (isnt_tok(p, tok_lbrace)) {
		checkout(simple_selector(p, ast_make(&p->ast, dest)));
	}
	return err_ok;
}

static ErrCode selector(Parser *p, Node *dest) {
	return composite_selector(p, dest);
}

static ErrCode selector_and_props(Parser *p, Node *dest) {
	node_set(dest, node_of(node_selector_and_props));
	checkout(selector(p, ast_make(&p->ast, dest)));
	return property_list(p, ast_make(&p->ast, dest));
}


static ErrCode call(Parser *p, Token name, Node *dest) {
	node_set(dest, node_from(node_call, name));

	checkout(give_tok(p, tok_lparen, NULL));
	
	while (isnt_tok(p, tok_rparen)) {
		// Parse other atoms
		checkout(value(p, ast_make(&p->ast, dest), 11));
		
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
		case tok_hexlit:			
			// This is a call
			if (current.type == tok_ident && is_tok(p, tok_lparen)) {
				// Set the node function name
				checkout(call(p, current, dest));
			}
			// This isn't a call
			else {
				node_set(dest, node_from(node_atom, current));
			}
		break;
		case tok_var:
			checkout(next_tok(p, &current));
			node_set(dest, node_from(node_var, current));
			break;
		case tok_operator: {			
			// Unary operators
			if (strncmp(current.val, "+", 1) == 0 ||
			    strncmp(current.val, "-", 1) == 0) {
				atom(p, ast_make(&p->ast, dest));
				node_set(dest, node_from(node_unary, current));
			}
			else {
				p->err = err_f(err_invalid_char, current.loc, "I can't use `%.*s' as a unary operator", current.len, current.val);
				return err_invalid_char;
			}
		} break;
		case tok_lparen:
			value(p, dest, 11);
			checkout(give_tok(p, tok_rparen, NULL));
			break;
		case tok_lbrace: 
			// TODO: Object literals
			p->note = err_f(err_note, current.loc, "There's no object literals yet\n");  
			__attribute__((fallthrough));			
		default:
			p->err = err_f(err_unexpected, current.loc, "I don't know what does %s do here", type_lookup[current.type]);
			return err_unexpected;
	}
	return err_ok;
}

// Operators ordered by their precedence
static const char* OPERATORS[11][4] = {
    { "*", "/", "%" },
    { "+", "-" },
    { "<<", ">>" },
    
    { "<", ">", ">=", "<=" },
    { "==", "!=" },
    { "&" },
    { "^" },
    
    { "|" },
    { "&&" },
    { "||" },
    { "=" },
};

static int precedence_of(Token tok) {
    for (int i = 0; i < 11; i += 1) {
        for (int j = 0; j < 4; j += 1) {
            if (OPERATORS[i][j] == NULL) continue;
            if (strncmp(tok.val, OPERATORS[i][j], tok.len) == 0) {
				return i + 1;
            }
        }
    }
    return 0;
}

static ErrCode value(Parser *p, Node *left, int prec) {
	// If precedence is zero then we parse the rest
	if (prec == 0) return atom(p, left);

	// Parse LHS
	checkout(value(p, left, prec-1));
	
	while (is_tok(p, tok_operator) && precedence_of(p->current) == prec) {
		// Skip the operator
		Token operator;
		checkout(give_tok(p, tok_operator, &operator));
	
		Node *new_node = ast_make(&p->ast, NULL);
		*new_node = *left;
		*left = node_from(node_binary, operator);
		left->first_child = new_node;
		checkout(value(p, ast_make(&p->ast, left), prec-1)); 
	}
	return err_ok;
}

// Parses a `x: y z;' property
static ErrCode property(Parser *p, Node *dest) {
	// Record savestate, because it can either be a property or a child! 
	Parser savestate = record(p);
	Token name;
	checkout(give_tok(p, tok_ident, &name));
	if (is_tok(p, tok_colon)) {
		// Skip colon
		checkout(next_tok(p, NULL));
		node_set(dest, node_from(node_property, name));
		checkout(value(p, ast_make(&p->ast, dest), 11));
		checkout(give_tok(p, tok_semicolon, NULL)); 
	}
	else {
		// Restore savestate
		*p = savestate;
		checkout(selector_and_props(p, dest));
	}
	return err_ok;
}

// Parses different pragmas, like @frame { ..code.. }
static ErrCode pragma(Parser *p, Node *dest) {
	checkout(give_tok(p, tok_at, NULL));
	
	Token pragma_name;
	checkout(give_tok(p, tok_ident, &pragma_name));
	node_set(dest, node_from(node_pragma, pragma_name));
	
	return property_list(p, dest);
}


static ErrCode property_list(Parser *p, Node *dest) {
	checkout(give_tok(p, tok_lbrace, NULL));
	node_set(dest, node_of(node_property_list));
	
	// Until we meet the closing brace
	while (isnt_tok(p, tok_rbrace)) {
		if (is_tok(p, tok_at)) {
			checkout(pragma(p, ast_make(&p->ast, dest)));
		}
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

