#include "common.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct {
	char *file;
	u32 line;
	u32 column;
} Location;

typedef enum {
	TOK_IDENTIFIER,
	TOK_OPERATOR,
	TOK_BRACKET,
	TOK_INTEGER,
	TOK_FLOAT,
	TOK_STRING,
	TOK_UNKNOWN,
	TOK_EOF,
} TokenType;

typedef struct {
	TokenType type;
	Location location;
	s32 indent_level;
	u8 *start;
	u32 len;
	union {
		struct {
			u64 number_int;
			b1 negative;
			u8 radix;
			u8 num_digits;
		};
		f64 number_float;
		char *str;
	};
} Token;

typedef struct {
	u8 *file;
	u32 index;
	u32 len;
	b32 last_not_newline;
	b32 in_indent;
	Location location;
} LexerState;

internal
s32 peek(LexerState *state) {
	if (state->index == state->len) {
		return -1;
	}
	return state->file[state->index];
}

internal
s32 next_char(LexerState *state) {
	if (state->last_not_newline) {
		state->location.column++;
	} else {
		state->location.line++;
		state->location.column = 1;
		state->in_indent = true;
	}
	if (state->index == state->len) {
		return -1;
	}
	s32 c = state->file[state->index++];
	if ((c & 0x80) != 0) {
		state->location.column--;
	}
	state->last_not_newline = c != '\n';
	return c;
}

// TODO UTF-8.
internal
b32 is_digit(s32 c) {
	return '0' <= c && c <= '9';
}

internal
b32 is_ident_start(s32 c) {
	return ('a' <= c && c <= 'z' ||
	        'A' <= c && c <= 'Z' ||
	        c == '_');
}

internal
b32 is_ident(s32 c) {
	return is_ident_start(c) || is_digit(c);
}

internal
b32 is_operator(s32 c) {
	return (c == '!' ||
	        c == '$' ||
	        c == '%' ||
	        c == '&' ||
	        c == '*' ||
	        c == '+' ||
	        c == ',' ||
	        c == '-' ||
	        c == '.' ||
	        c == '/' ||
	        c == ':' ||
	        c == ';' ||
	        c == '<' ||
	        c == '=' ||
	        c == '>' ||
	        c == '?' ||
	        c == '@' ||
	        c == '\\' ||
	        c == '^' ||
	        c == '`' ||
	        c == '|' ||
	        c == '~');
}

internal
b32 is_bracket(s32 c) {
	return (c == '{' || c == '}' ||
	        c == '(' || c == ')' ||
	        c == '[' || c == ']');
}

internal
void next_token(LexerState *state, Token *token) {
	s32 c;
	// TODO comments!
	// TODO alignment (we don't care about the value, but it must exist). A stack? Or just check it exists...
	do {
		c = next_char(state);
		if (state->in_indent) {
			token->indent_level = 0;
			while (c == '\t') {
				token->indent_level++;
				c = next_char(state);
			}
		} else {
			token->indent_level = -1;
		}
		if (c == -1) {
			token->type = TOK_EOF;
			token->location = state->location;
			return;
		}
		while (c == ' ') {
			c = next_char(state);
		}
	} while (c == '\n');
	token->location = state->location;
	token->start = state->file + state->index - 1;
	token->len = 1;
#define ADD_WHILE(expr) \
	c = peek(state); \
	while (expr) { \
		next_char(state); \
		token->len++; \
		c = peek(state); \
	}
	if (is_ident_start(c)) {
		token->type = TOK_IDENTIFIER;
		ADD_WHILE(is_ident(c));
	} else if (is_operator(c)) {
		token->type = TOK_OPERATOR;
		ADD_WHILE(is_operator(c));
	} else if (is_bracket(c)) {
		token->type = TOK_BRACKET;
	} else if (is_digit(c)) {
		token->type = TOK_INTEGER;
		ADD_WHILE(is_digit(c));
		// TODO actually parse float
		if (c == 'b' || c == 'o' || c == 'd' || c == 'x' || c == 'r') {
			next_char(state);
			// TODO handle radix
			ADD_WHILE(is_digit(c));
		}
		if (c == '.') {
			token->type = TOK_FLOAT;
			// TODO allow radix for float?
			next_char(state);
			ADD_WHILE(is_digit(c));
		}
		if (c == 'e') {
			token->type = TOK_FLOAT;
			next_char(state);
			ADD_WHILE(is_digit(c));
		}
	} else if (c == '"') {
		// TODO handle escapes, figure out multiline strategy
		token->type = TOK_STRING;
		ADD_WHILE(c != '"' && c != '\n' && c != -1);
	} else {
		token->type = TOK_UNKNOWN;
	}
}


internal
void usage() {
	fprintf(stderr,
	        "usage: mepa <command> [<options>] [<args>]\n"
	        "\n"
	        "Commands:\n"
	        "    help             - show this usage text\n"
	        "    format [<file>]  - reformat code\n");
}

internal __attribute__((noreturn))
void die() {
	perror("mepa");
	exit(1);
}

internal
void *nonnull_or_die(void *ptr) {
	if (ptr == NULL) {
		die();
	}
	return ptr;
}

internal
int help_main(int argc, char **argv) {
	(void) argc;
	(void) argv;
	usage();
	return 0;
}

internal
int format_main(int argc, char **argv) {
	FILE *file;
	char *fname;
	if (argc == 2 || strcmp(argv[2], "-") == 0) {
		file = stdin;
		fname = "<stdin>";
	} else {
		file = nonnull_or_die(fopen(argv[2], "r"));
		fname = argv[2];
	}
	u32 cap = 4096;
	u32 size = 0;
	u8 *contents = malloc(cap);
	while (true) {
		u32 wanted = cap - size;
		usize result = fread(contents + size, 1, wanted, file);
		if (result != wanted) {
			if (ferror(file)) {
				die();
			}
			size += result;
			break;
		}
		size += result;
		cap += cap >> 1;
		contents = realloc(contents, cap);
	}
	contents = realloc(contents, size);
	fclose(file);
	LexerState state = {};
	state.file = contents;
	state.len = size;
	state.location.file = fname;
	Token token = { .type = TOK_UNKNOWN };
	while (token.type != TOK_EOF) {
		next_token(&state, &token);
		printf("%s:%u:%u: (indent %d) ", token.location.file, token.location.line, token.location.column, token.indent_level);
		switch (token.type) {
		case TOK_IDENTIFIER: printf("IDENTIFIER \"%.*s\"", token.len, token.start); break;
		case TOK_OPERATOR: printf("OPERATOR \"%.*s\"", token.len, token.start); break;
		case TOK_BRACKET: printf("BRACKET '%c'", *token.start); break;
		case TOK_INTEGER: printf("INTEGER \"%.*s\"", token.len, token.start); break;
		case TOK_FLOAT: printf("FLOAT \"%.*s\"", token.len, token.start); break;
		case TOK_STRING: printf("STRING \"%.*s\"", token.len, token.start); break;
		case TOK_UNKNOWN: printf("UNKNOWN \"%.*s\"", token.len, token.start); break;
		case TOK_EOF: printf("EOF"); break;
		}
		printf("\n");
	}
	return 0;
}

typedef struct {
	char *command;
	int (*func)(int, char**);
} Command;

internal
Command commands[] = {
	{"help",   help_main},
	{"-h",     help_main},
	{"--help", help_main},
	{"-help",  help_main},
	{"format", format_main},
};

int main(int argc, char **argv) {
	if (argc == 1) {
		return help_main(argc, argv);
	}
	for (u32 i = 0; i < array_count(commands); i++) {
		if (strcmp(argv[1], commands[i].command) == 0) {
			return commands[i].func(argc, argv);
		}
	}
	fprintf(stderr, "Unknown command %s\n", argv[1]);
	usage();
	return 1;
}
