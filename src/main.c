#include "unicode.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct {
	char *file;
	u32 line;
	u32 column;
} Location;

typedef enum {
	TOK_START_LETTERS,
	TOK_CONTINUE_LETTERS,
	TOK_DIGITS,
	TOK_SYMBOL,
	TOK_BRACKET,
	TOK_STRING,
	TOK_NEWLINE,
	TOK_SPACES,
	TOK_COMMENT,
	TOK_UNKNOWN,
	TOK_EOF,
} TokenType;

typedef struct {
	TokenType type;
	Location location;
	u8 *start;
	u32 len;
	char *str;
} Token;

typedef enum {
	LEX_ERROR_UTF8_BOM_NOT_ALLOWED,
	LEX_ERROR_UTF8_UNEXPECTED_CONTINUATION_CHAR,
	LEX_ERROR_UTF8_OVERLONG_SEQUENCE,
	LEX_ERROR_UTF8_EOF_IN_SEQUENCE,
	LEX_ERROR_UTF8_SEQUENCE_TOO_SHORT,
	LEX_ERROR_UTF8_WRONG_SEQUENCE_LENGTH,
	LEX_ERROR_UTF8_CODE_POINT_TOO_HIGH,
	LEX_ERROR_EOF_IN_COMMENT,
	LEX_ERROR_EOF_IN_STRING,
	LEX_ERROR_UNTERMINATED_STRING,
} LexerErrorType;

internal char *lexer_error_messages[] = {
	"Invalid UTF-8 encoding: byte order markers are not permitted",
	"Invalid UTF-8 encoding: unexpected continuation byte",
	"Invalid UTF-8 encoding: sequence too long (> 4 bytes)",
	"Invalid UTF-8 encoding: hit EOF while decoding sequence",
	"Invalid UTF-8 encoding: too few of continuation bytes in sequence",
	"Invalid UTF-8 encoding: sequence length too long for code point",
	"Invalid UTF-8 encoding: code point exceeded limit of 0x10ffff",
	"Hit EOF while looking for end of comment",
	"Hit EOF while looking for end of string",
	"String was not terminated before end of line",
};

typedef struct {
	LexerErrorType type;
	Location location_start;
	Location location_end;
} LexerError;

typedef struct {
	u8 *file;
	u32 index;
	u32 len;
	b32 last_not_newline;
	b32 in_indent;
	u8 **lines;
	Location location;
	LexerError *errors;
	u32 error_count;
	u32 error_cap;
} LexerState;

internal
void report_error_single_line(LexerState *state, LexerErrorType type, Location location, u32 column_end) {
	if (!state->errors) {
		state->error_cap = 8;
		state->errors = malloc(state->error_cap * sizeof(LexerError));
	}
	if (state->error_count == state->error_cap) {
		state->error_cap += state->error_cap >> 1;
		state->errors = realloc(state->errors, state->error_cap * sizeof(LexerError));
	}
	LexerError *error = &state->errors[state->error_count++];
	error->type = type;
	error->location_start = location;
	error->location_end = location;
	error->location_end.column = column_end;
}

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
		state->lines[state->location.line - 1] = state->file + state->index;
		state->in_indent = true;
	}
retry:
	if (state->index == state->len) {
		return -1;
	}
	b32 first_char = state->index == 0;
	s32 c = state->file[state->index++];
	state->last_not_newline = c != '\n';
	if (c == 0xff) {
		report_error_single_line(state, LEX_ERROR_UTF8_OVERLONG_SEQUENCE,
		                         state->location, state->location.column);
		// TODO skip continuation bytes here
		goto retry;
	}
	u32 count = (u32)__builtin_clz((u32)c ^ 0xff) - 24;
	if (count == 1) {
		report_error_single_line(state, LEX_ERROR_UTF8_UNEXPECTED_CONTINUATION_CHAR,
		                         state->location, state->location.column);
		goto retry;
	} else if (count > 4) {
		report_error_single_line(state, LEX_ERROR_UTF8_OVERLONG_SEQUENCE,
		                         state->location, state->location.column);
		// TODO skip continuation bytes here
		goto retry;
	} else if (count != 0) {
		u8 chars[count];
		chars[0] = (u8)c;
		chars[0] <<= count;
		chars[0] >>= count;
		for (u32 i = 1; i < count; i++) {
			if (state->index == state->len) {
				report_error_single_line(state, LEX_ERROR_UTF8_EOF_IN_SEQUENCE,
				                         state->location, state->location.column);
				return -1;
			}
			chars[i] = state->file[state->index++];
			if ((chars[i] >> 6) != 2) {
				report_error_single_line(state, LEX_ERROR_UTF8_SEQUENCE_TOO_SHORT,
				                         state->location, state->location.column);
				goto retry;
			}
			chars[i] &= 0x7f;
		}
		c = chars[0] << (count - 1) * 6;
		for (u32 i = 1; i < count; i++) {
			c |= chars[i] << (count - i - 1) * 6;
		}
		if (first_char && c == 0xfeff) {
			report_error_single_line(state, LEX_ERROR_UTF8_BOM_NOT_ALLOWED,
			                         state->location, state->location.column);
			goto retry;
		}
		if (c <= 0x7f ||
		    count > 2 && c <= 0x7ff ||
		    count > 3 && c <= 0xffff) {
			report_error_single_line(state, LEX_ERROR_UTF8_WRONG_SEQUENCE_LENGTH,
			                         state->location, state->location.column);
			goto retry;
		}
		if (c > 0x10ffff) {
			report_error_single_line(state, LEX_ERROR_UTF8_CODE_POINT_TOO_HIGH,
			                         state->location, state->location.column);
			goto retry;
		}
	}
	return c;
}

internal
b32 in_unicode_range(u32 c, UnicodeRange16 *table16, u32 size16, UnicodeRange32 *table32, u32 size32) {
	if (c <= 0xff) {
		for (u32 i = 0; i < size16; i++) {
			UnicodeRange16 range = table16[i];
			if (range.start > c) {
				return false;
			}
			if (range.start <= c && c <= range.end) {
				return true;
			}
		}
		return false;
	}
	if (c <= 0xffff) {
		u32 lo = 0;
		u32 hi = size16 - 1;
		while (lo < hi) {
			u32 mid = (lo + hi) / 2;
			if (table16[mid].end < c) {
				lo = mid + 1;
			} else {
				hi = mid;
			}
		}
		UnicodeRange16 range = table16[lo];
		return range.start <= c && c <= range.end;
	}
	u32 lo = 0;
	u32 hi = size32 - 1;
	while (lo < hi) {
		u32 mid = (lo + hi) / 2;
		if (table32[mid].end < c) {
			lo = mid + 1;
		} else {
			hi = mid;
		}
	}
	UnicodeRange32 range = table32[lo];
	return range.start <= c && c <= range.end;
}

internal
b32 is_start_letter(s32 c) {
	if (c == -1) {
		return false;
	}
	return in_unicode_range((u32)c,
	                        xid_start_table_16, array_count(xid_start_table_16),
	                        xid_start_table_32, array_count(xid_start_table_32));
}

internal
b32 is_continue_letter(s32 c) {
	if (c == -1) {
		return false;
	}
	return in_unicode_range((u32)c,
	                        xid_continue_table_16, array_count(xid_continue_table_16),
	                        xid_continue_table_32, array_count(xid_continue_table_32));
}

internal
b32 is_symbol(s32 c) {
	return (c == '!' ||
	        c == '#' ||
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
	        c == '_' ||
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
	c = next_char(state);
	if (c == -1) {
		token->type = TOK_EOF;
		token->location = state->location;
		return;
	}
#define ADD_WHILE(expr) \
	c = peek(state); \
	while (expr) { \
		next_char(state); \
		token->len++; \
		c = peek(state); \
	}
	// TODO state->file is in bytes, not codepoints
	token->start = state->file + state->index - 1;
	token->len = 1;
	if (c == '\n') {
		token->type = TOK_NEWLINE;
		ADD_WHILE(c == '\t');
		token->location = state->location;
		if (token->len > 1) {
			token->location.column = 1;
		}
		return;
	}
	token->location = state->location;
	if (c == ' ') {
		token->type = TOK_SPACES;
		ADD_WHILE(c == ' ');
		return;
	}
	if (c == '/') {
		s32 next = peek(state);
		if (next == '/') {
			token->type = TOK_COMMENT;
			ADD_WHILE(c != '\n' && c != -1);
			return;
		} else if (next == '*') {
			token->type = TOK_COMMENT;
			next_char(state);
			token->len++;
			u32 depth = 1;
			while (depth > 0) {
				c = peek(state);
				if (c == -1) {
					report_error_single_line(state, LEX_ERROR_EOF_IN_COMMENT,
					                         token->location, token->location.column + 1);
					return;
				}
				next_char(state);
				token->len++;
				if (c == '/') {
					next = peek(state);
					if (next == '*') {
						next_char(state);
						token->len++;
						depth++;
					}
				} else if (c == '*') {
					next = peek(state);
					if (next == '/') {
						next_char(state);
						token->len++;
						depth--;
					}
				}
			}
			return;
		}
	}
	if ('0' <= c && c <= '9') {
		token->type = TOK_DIGITS;
		ADD_WHILE('0' <= c && c <= '9');
		return;
	}
	if (is_symbol(c)) {
		token->type = TOK_SYMBOL;
		return;
	}
	if (is_bracket(c)) {
		token->type = TOK_BRACKET;
		return;
	}
	if (is_start_letter(c)) {
		token->type = TOK_START_LETTERS;
		ADD_WHILE(is_start_letter(c));
		return;
	}
	if (is_continue_letter(c)) {
		token->type = TOK_CONTINUE_LETTERS;
		ADD_WHILE(!is_start_letter(c) && is_continue_letter(c));
		return;
	}
	if (c == '"') {
		token->type = TOK_STRING;
		while (true) {
			c = peek(state);
			if (c == -1) {
				report_error_single_line(state, LEX_ERROR_EOF_IN_STRING,
				                         token->location, state->location.column);
				break;
			}
			if (c == '\n') {
				report_error_single_line(state, LEX_ERROR_UNTERMINATED_STRING,
				                         token->location, state->location.column);
				break;
			}
			next_char(state);
			token->len++;
			if (c == '"') {
				break;
			}
			if (c == '\\') {
				c = peek(state);
				if (c == '"') {
					next_char(state);
					token->len++;
				}
			}
		}
		return;
	}
	token->type = TOK_UNKNOWN;
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
	u32 lines = 0;
	while (true) {
		u32 wanted = cap - size;
		usize result = fread(contents + size, 1, wanted, file);
		b32 done = false;
		if (result != wanted) {
			if (ferror(file)) {
				die();
			}
			done = true;
		}
		for (u32 i = size; i < size + result; i++) {
			if (contents[i] == '\n') {
				lines++;
			}
		}
		size += result;
		if (done) {
			break;
		}
		cap += cap >> 1;
		contents = realloc(contents, cap);
	}
	if (size == 0) {
		return 0;
	}
	contents = realloc(contents, size);
	fclose(file);
	if (contents[size - 1] != '\n') {
		lines++;
	}
	LexerState state = {};
	state.file = contents;
	state.len = size;
	state.location.file = fname;
	state.lines = malloc(lines * sizeof(u8 *));
	Token token = { .type = TOK_UNKNOWN };
	while (token.type != TOK_EOF) {
		next_token(&state, &token);
		printf("%s:%u:%u: ", token.location.file, token.location.line, token.location.column);
		switch (token.type) {
		case TOK_START_LETTERS: printf("START_LETTERS \"%.*s\"", token.len, token.start); break;
		case TOK_CONTINUE_LETTERS: printf("CONTINUE_LETTERS \"%.*s\"", token.len, token.start); break;
		case TOK_DIGITS: printf("DIGITS \"%.*s\"", token.len, token.start); break;
		case TOK_SYMBOL: printf("SYMBOL '%.*s'", token.len, token.start); break;
		case TOK_BRACKET: printf("BRACKET '%.*s'", token.len, token.start); break;
		case TOK_STRING: printf("STRING \"%.*s\"", token.len, token.start); break;
		case TOK_NEWLINE: printf("NEWLINE indent=%u", token.len - 1); break;
		case TOK_SPACES: printf("SPACES \"%.*s\"", token.len, token.start); break;
		case TOK_COMMENT: printf("COMMENT \"%.*s\"", token.len, token.start); break;
		case TOK_UNKNOWN: printf("UNKNOWN \"%.*s\"", token.len, token.start); break;
		case TOK_EOF: printf("EOF"); break;
		}
		printf("\n");
	}
	for (u32 i = 0; i < state.error_count; i++) {
		LexerError *error = &state.errors[i];
		if (error->location_start.line == error->location_end.line) {
			fprintf(stderr, "%s:%d:%d",
			        error->location_start.file,
			        error->location_start.line,
			        error->location_start.column);
			if (error->location_start.column != error->location_end.column) {
				fprintf(stderr, "-%d", error->location_end.column);
			}
			fprintf(stderr, ": %s\n", lexer_error_messages[error->type]);
			u8 *line = state.lines[error->location_start.line - 1];
			u8 *end = (u8 *) strchr((char *)line, '\n');
			fprintf(stderr, "%.*s\n", (u32) (end - line), line);
			for (u32 j = 1; j < error->location_start.column; j++) {
				if (line[j - 1] == '\t') {
					fprintf(stderr, "        ");
				} else {
					fputc(' ', stderr);
				}
			}
			for (u32 j = error->location_start.column; j <= error->location_end.column; j++) {
				fputc('^', stderr);
			}
			fputc('\n', stderr);
		} else {
			assert("Multi line errors not yet implemented" == 0);
		}
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
