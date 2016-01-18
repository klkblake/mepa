#include "unicode.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <getopt.h>
#include <errno.h>
#include <sysexits.h>

typedef struct {
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
	LEX_ERROR_EOF_IN_COMMENT,
	LEX_ERROR_EOF_IN_STRING,
	LEX_ERROR_UNTERMINATED_STRING,
} LexerErrorType;

internal char *lexer_error_messages[] = {
	"Hit EOF while looking for end of comment",
	"Hit EOF while looking for end of string",
	"String was not terminated before end of line",
};

typedef struct {
	Location start;
	Location end;
} Range;

typedef struct {
	LexerErrorType type;
	Location location;
	Range range;
} LexerError;

typedef struct {
	char *name;
	u8 *data;
	u32 len;
	u8 **lines;
} SourceFile;

typedef struct {
	u32 count;
	u32 limit;
} ErrorState;

typedef struct {
	SourceFile file;
	u32 index;
	b32 last_not_newline;
	Location location;
	ErrorState *errors;
} LexerState;

internal
void report_error(ErrorState *state, SourceFile *file, LexerErrorType type, Location location, Range range) {
	state->count++;
	if (state->limit && state->count > state->limit) {
		return;
	}
	fprintf(stderr, "%s:%d:%d: %s\n", file->name, location.line, location.column, lexer_error_messages[type]);
	u8 *line = file->lines[location.line - 1];
	u8 *end = (u8 *) strchrnul((char *)line, '\n');
	u32 size = (u32) (end - line);
	// TODO should we trust this to not overflow the stack?
	u8 buf[size * 8];
	u32 len = 0;
	for (u32 i = 0; i < size; i++) {
		if (line[i] == '\t') {
			for (u32 k = 0; k < 8; k++) {
				buf[len++] = ' ';
			}
		} else {
			buf[len++] = line[i];
		}
	}
	fprintf(stderr, "%.*s\n", len, buf);
	// TODO handle utf-8!
	u32 col_start = 0;
	if (range.start.line == location.line) {
		col_start = range.start.column - 1;
	}
	u32 col_end = size - 1;
	if (range.end.line == location.line) {
		col_end = range.end.column - 1;
	}
	len = 0;
	for (u32 i = 0; i < size; i++) {
		u8 c;
		if (i == location.column - 1) {
			c = '^';
		} else if (col_start <= i && i <= col_end) {
			c = '~';
		} else {
			c = ' ';
		}
		if (line[i] == '\t') {
			for (u32 k = 0; k < 8; k++) {
				buf[len++] = c;
			}
		} else {
			buf[len++] = c;
		}
	}
	fprintf(stderr, "%.*s\n", len, buf);
}

internal
s32 peek(LexerState *state) {
	if (state->index == state->file.len) {
		return -1;
	}
	s32 c = state->file.data[state->index];
	u32 seq_len = (u32)__builtin_clz((u32)c ^ 0xff) - 24;
	if (seq_len > 0) {
		u32 index = state->index + 1;
		c &= 0xff >> seq_len;
		for (u32 i = 0; i < seq_len - 1; i++) {
			c <<= 6;
			c |= state->file.data[index++] & 0x7f;
		}
	}
	return c;
}

internal
void advance(LexerState *state) {
	if (state->last_not_newline) {
		state->location.column++;
	} else {
		state->location.line++;
		state->location.column = 1;
	}
	if (state->index < state->file.len) {
		s32 c = state->file.data[state->index++];
		state->last_not_newline = c != '\n';
		u32 seq_len = (u32)__builtin_clz((u32)c ^ 0xff) - 24;
		if (seq_len > 0) {
			state->index += seq_len - 1;
		}
	}
}

internal
s32 next_char(LexerState *state) {
	s32 c = peek(state);
	advance(state);
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
	token->start = state->file.data + state->index;
	c = next_char(state);
	token->len = (u32) (state->file.data + state->index - token->start);
	if (c == -1) {
		token->type = TOK_EOF;
		token->location = state->location;
		return;
	}
#define ADD_WHILE(expr) \
	c = peek(state); \
	while (expr) { \
		advance(state); \
		c = peek(state); \
	} \
	token->len = (u32) (state->file.data + state->index - token->start)
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
#define REPORT_ERROR(code) \
	report_error(state->errors, &state->file, code, \
	             token->location, (Range){token->location, state->location})
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
			u32 depth = 1;
			while (depth > 0) {
				c = peek(state);
				if (c == -1) {
					REPORT_ERROR(LEX_ERROR_EOF_IN_COMMENT);
					break;
				}
				next_char(state);
				if (c == '/') {
					next = peek(state);
					if (next == '*') {
						advance(state);
						depth++;
					}
				} else if (c == '*') {
					next = peek(state);
					if (next == '/') {
						advance(state);
						depth--;
					}
				}
			}
			token->len = (u32) (state->file.data + state->index - token->start);
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
				REPORT_ERROR(LEX_ERROR_EOF_IN_STRING);
				break;
			}
			if (c == '\n') {
				REPORT_ERROR(LEX_ERROR_UNTERMINATED_STRING);
				break;
			}
			advance(state);
			if (c == '"') {
				break;
			}
			if (c == '\\') {
				c = peek(state);
				if (c == '"') {
					advance(state);
				}
			}
		}
		token->len = (u32) (state->file.data + state->index - token->start);
		return;
	}
	token->type = TOK_UNKNOWN;
#undef REPORT_ERROR
#undef ADD_WHILE
}

typedef enum {
	UTF8_ERROR_BOM_NOT_ALLOWED,
	UTF8_ERROR_UNEXPECTED_CONTINUATION_CHAR,
	UTF8_ERROR_OVERLONG_SEQUENCE,
	UTF8_ERROR_EOF_IN_SEQUENCE,
	UTF8_ERROR_SEQUENCE_TOO_SHORT,
	UTF8_ERROR_WRONG_SEQUENCE_LENGTH,
	UTF8_ERROR_SURROGATES_NOT_ALLOWED,
	UTF8_ERROR_CODE_POINT_TOO_HIGH,
} UTF8ErrorType;

internal
char *utf8_error_messages[] = {
	"byte order markers are not permitted",
	"unexpected continuation byte",
	"sequence too long (> 4 bytes)",
	"hit EOF while decoding sequence",
	"too few continuation bytes in sequence",
	"sequence length too long for code point",
	"surrogates are not permitted",
	"code point exceeded limit of 0x10ffff",
};

typedef struct {
	UTF8ErrorType type;
	Location location;
	u8 *line;
} UTF8Error;

typedef struct UTF8Errors {
	UTF8Error *errors;
	u32 count;
	u32 cap;
	u32 limit;
} UTF8Errors;

internal
void report_utf8_error_(UTF8Errors *errors, UTF8ErrorType type, Location location, u8 *line) {
	if (!errors->errors) {
		errors->cap = 8;
		errors->errors = malloc(errors->cap * sizeof(UTF8Error));
	}
	if (errors->limit && errors->count >= errors->limit) {
		errors->count++;
		return;
	}
	if (errors->count == errors->cap) {
		errors->cap += errors->cap >> 1;
		errors->errors = realloc(errors->errors, errors->cap * sizeof(UTF8Error));
	}
	UTF8Error *error = &errors->errors[errors->count++];
	error->type = type;
	error->location = location;
	error->line = line;
}

internal
u8 *validate_utf8(SourceFile *file, UTF8Errors *errors) {
	u32 index = 0;
	b32 last_not_newline = false;
	Location location = {};
#define report_utf8_error(type) report_utf8_error_(errors, type, location, file->lines[location.line - 1])
	while (index < file->len) {
		if (last_not_newline) {
			location.column++;
		} else {
			location.line++;
			location.column = 1;
			file->lines[location.line - 1] = file->data + index;
		}
		while (index < file->len) {
			b32 first_char = index == 0;
			s32 c = file->data[index++];
			last_not_newline = c != '\n';
			if (c == 0xff) {
				report_utf8_error(UTF8_ERROR_OVERLONG_SEQUENCE);
				while (index < file->len && file->data[index] >> 6 == 2) {
					index++;
				}
				continue;
			}
			u32 count = (u32)__builtin_clz((u32)c ^ 0xff) - 24;
			if (count == 1) {
				report_utf8_error(UTF8_ERROR_UNEXPECTED_CONTINUATION_CHAR);
				continue;
			} else if (count > 4) {
				report_utf8_error(UTF8_ERROR_OVERLONG_SEQUENCE);
				while (index < file->len && file->data[index] >> 6 == 2) {
					index++;
				}
				continue;
			} else if (count != 0) {
				u8 chars[count];
				chars[0] = (u8)c;
				chars[0] <<= count;
				chars[0] >>= count;
				for (u32 i = 1; i < count; i++) {
					if (index == file->len) {
						report_utf8_error(UTF8_ERROR_EOF_IN_SEQUENCE);
						break;
					}
					chars[i] = file->data[index++];
					if ((chars[i] >> 6) != 2) {
						report_utf8_error(UTF8_ERROR_SEQUENCE_TOO_SHORT);
						continue;
					}
					chars[i] &= 0x7f;
				}
				c = chars[0] << (count - 1) * 6;
				for (u32 i = 1; i < count; i++) {
					c |= chars[i] << (count - i - 1) * 6;
				}
				if (first_char && c == 0xfeff) {
					report_utf8_error(UTF8_ERROR_BOM_NOT_ALLOWED);
					continue;
				}
				if (c <= 0x7f ||
				    count > 2 && c <= 0x7ff ||
				    count > 3 && c <= 0xffff) {
					report_utf8_error(UTF8_ERROR_WRONG_SEQUENCE_LENGTH);
					continue;
				}
				if (c >= 0xd800 && c <= 0xdfff) {
					report_utf8_error(UTF8_ERROR_SURROGATES_NOT_ALLOWED);
					continue;
				}
				if (c > 0x10ffff) {
					report_utf8_error(UTF8_ERROR_CODE_POINT_TOO_HIGH);
					continue;
				}
			}
			break;
		}
	}
	return file->data;
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
void die(u8 code) {
	perror("mepa");
	exit(code);
}

internal
void *nonnull_or_die(void *ptr, u8 code) {
	if (ptr == NULL) {
		die(code);
	}
	return ptr;
}

internal
int help_main(int argc, char *argv[static argc]) {
	(void) argc;
	(void) argv;
	usage();
	return 0;
}

internal
int format_main(int argc, char *argv[static argc]) {
	FILE *file_stream;
	SourceFile file = {};
	ErrorState error_state = {}; // TODO rename when it stops clashing
	error_state.limit = 20;

	const int CODE_ERROR_LIMIT = 256;
	struct option longopts[] = {
		{ "error-limit", required_argument, NULL, CODE_ERROR_LIMIT },
		{ NULL, 0, NULL, 0 },
	};
	int option;
	while ((option = getopt_long(argc, argv, "", longopts, NULL)) != -1) {
		switch (option) {
		case 0: continue;
		case CODE_ERROR_LIMIT:
		{
			if (*optarg == 0) {
				fprintf(stderr, "Empty argument to --error-limit\n");
				return EX_USAGE;
			}
			char *endptr;
			errno = 0;
			s64 value = strtol(optarg, &endptr, 10);
			if (*endptr != 0) {
				fprintf(stderr, "Invalid number in argument to --error-limit\n");
				return EX_USAGE;
			}
			if (errno == ERANGE || value < 0 || value >= (1ll << 32)) {
				fprintf(stderr, "Argument to --error-limit out of range\n");
				return EX_USAGE;
			}
			error_state.limit = (u32)value;
			break;
		}
		case '?': return EX_USAGE;
		}
	}
	if (argc - optind > 1) {
		fprintf(stderr, "format expects exactly one file");
		return EX_USAGE;
	}
	if (optind == argc || strcmp(argv[optind], "-") == 0) {
		file_stream = stdin;
		file.name = "<stdin>";
	} else {
		file_stream = nonnull_or_die(fopen(argv[optind], "r"), EX_NOINPUT);
		file.name = argv[optind];
	}
	u32 cap = 4096;
	file.data = malloc(cap);
	u32 lines = 0;
	while (true) {
		u32 wanted = cap - file.len;
		usize result = fread(file.data + file.len, 1, wanted, file_stream);
		b32 done = false;
		if (result != wanted) {
			if (ferror(file_stream)) {
				die(EX_NOINPUT);
			}
			done = true;
		}
		for (u32 i = file.len; i < file.len + result; i++) {
			if (file.data[i] == '\n') {
				lines++;
			}
		}
		file.len += result;
		if (done) {
			break;
		}
		u32 newcap = cap + (cap >> 1);
		if (newcap < cap) {
			fprintf(stderr, "%s exceeds maximum file size of 4GB\n", file.name);
			return EX_DATAERR;
		}
		cap = newcap;
		file.data = realloc(file.data, cap);
	}
	fclose(file_stream);
	if (file.len == 0) {
		return 0;
	}
	if (file.data[file.len - 1] != '\n') {
		lines++;
	}
	file.data = realloc(file.data, file.len);
	file.lines = malloc(lines * sizeof(u8 *));
	UTF8Errors errors = {};
	errors.limit = error_state.limit;
	file.data = validate_utf8(&file, &errors);
	u32 num_errors = errors.count;
	if (errors.limit && errors.count > errors.limit) {
		num_errors = errors.limit;
	}
	for (u32 i = 0; i < num_errors; i++) {
		UTF8Error *error = &errors.errors[i];
		fprintf(stderr, "%s:%d:%d: Invalid UTF-8 encoding: %s\n",
		        file.name, error->location.line, error->location.column, utf8_error_messages[error->type]);
		u32 line_size = (u32)((u8 *)strchrnul((char *)error->line, '\n') - error->line);
		u8 buf[line_size * 3];
		u32 buf_size = 0;
		for (u32 j = 0; j < line_size; j++) {
			u8 c = error->line[j];
			if (c < ' ' && c != '\t' || c == 0x7f) {
				// Map control codes to control pictures
				buf[buf_size++] = 0xe2;
				buf[buf_size++] = 0x90;
				buf[buf_size++] = 0x80 | c;
			} else {
				buf[buf_size++] = c;
			}
		}
		fprintf(stderr, "%.*s\n", buf_size, buf);
	}
	if (num_errors == errors.count) {
		fprintf(stderr, "%u errors\n", num_errors);
	} else {
		fprintf(stderr, "%u errors, only first %u reported\n", errors.count, errors.limit);
	}
	if (errors.count > 0) {
		return 1;
	}
	LexerState state = {};
	state.errors = &error_state;
	state.file = file;
	Token token = { .type = TOK_UNKNOWN };
	while (token.type != TOK_EOF) {
		next_token(&state, &token);
		printf("%s:%u:%u: ", file.name, token.location.line, token.location.column);
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
		usage();
		return EX_USAGE;
	}
	for (u32 i = 0; i < array_count(commands); i++) {
		if (strcmp(argv[1], commands[i].command) == 0) {
			return commands[i].func(argc - 1, argv + 1);
		}
	}
	fprintf(stderr, "Unknown command %s\n", argv[1]);
	usage();
	return EX_USAGE;
}
