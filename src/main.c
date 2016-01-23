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
	TOK_WORD,
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
} Token;

typedef enum {
	ERROR_UTF8_BOM_NOT_ALLOWED,
	ERROR_UTF8_UNEXPECTED_CONTINUATION_CHAR,
	ERROR_UTF8_OVERLONG_SEQUENCE,
	ERROR_UTF8_EOF_IN_SEQUENCE,
	ERROR_UTF8_SEQUENCE_TOO_SHORT,
	ERROR_UTF8_WRONG_SEQUENCE_LENGTH,
	ERROR_UTF8_SURROGATES_NOT_ALLOWED,
	ERROR_UTF8_CODE_POINT_TOO_HIGH,

	ERROR_LEX_EOF_IN_COMMENT,
	ERROR_LEX_EOF_IN_STRING,
	ERROR_LEX_UNTERMINATED_STRING,
} LexerErrorType;

internal char *error_messages[] = {
	// TODO parameterise some of these
	"byte order markers are not permitted",
	"unexpected continuation byte",
	"sequence too long (> 4 bytes)",
	"hit EOF while decoding sequence",
	"too few continuation bytes in sequence",
	"sequence length too long for code point",
	"surrogates are not permitted",
	"code point exceeded limit of 0x10ffff",

	"Hit EOF while looking for end of comment",
	"Hit EOF while looking for end of string",
	"String was not terminated before end of line",
};

typedef struct {
	Location start;
	Location end;
} Range;

typedef struct {
	char *name;
	u8 *data;
	u32 len;
	u32 *lines;
} SourceFile;

typedef struct {
	u32 count;
	u32 limit;
} ErrorCount;

typedef struct {
	SourceFile file;
	u32 index;
	b32 last_not_newline;
	b32 last_tab;
	Location location;
	ErrorCount *errors;
} LexerState;

internal
void report_error(ErrorCount *errors, SourceFile *file, LexerErrorType type, Location location, Range range) {
	errors->count++;
	if (errors->limit && errors->count > errors->limit) {
		return;
	}
	fprintf(stderr, "%s:%d:%d: %s\n", file->name, location.line, location.column, error_messages[type]);
	u8 *line = file->data + file->lines[location.line - 1];
	u8 *end = (u8 *) strchr((char *)line, '\n');
	u32 size = (u32) (end - line);
	// TODO should we trust this to not overflow the stack?
	u8 buf[size * 8];
	u32 num_cols = 0;
	u32 len = 0;
	for (u32 i = 0; i < size; i++) {
		// TODO Map C1 codes (code points 0x80-0x9f, utf-8 0xc2 0x80 to 0xc2 9f) to the replacement character
		// TODO sanitise against malformed UTF-8
		if (line[i] == '\t') {
			for (u32 k = 0; k < 8; k++) {
				buf[len++] = ' ';
			}
			num_cols += 7;
		} else if (line[i] < ' ') {
			// Map C0 control codes to control pictures
			buf[len++] = 0xe2;
			buf[len++] = 0x90;
			buf[len++] = 0x80 | line[i];
		} else if (line[i] == 0x7f) {
			// Map DEL to its control picture
			buf[len++] = 0xe2;
			buf[len++] = 0x90;
			buf[len++] = 0xa1;
		} else {
			buf[len++] = line[i];
		}
		num_cols++;
	}
	fprintf(stderr, "%.*s\n", len, buf);
	// TODO handle utf-8!
	// TODO handle invalid utf-8 iff we are reporting a UTF-8 error
	assert(location.line || !range.start.line && !range.end.line);
	assert(!range.start.line || range.start.line <= location.line);
	assert(!range.end.line || range.end.line >= location.line);
	u32 col_start = 0;
	if (range.start.line == 0) {
		col_start = -1u;
	} else if (range.start.line == location.line) {
		col_start = range.start.column - 1;
	}
	u32 col_end = num_cols - 1;
	if (range.end.line == location.line) {
		col_end = range.end.column - 1;
	}
	len = 0;
	for (u32 i = 0, col = 0; i < size; i++) {
		u8 c;
		if (col == location.column - 1) {
			c = '^';
		} else if (col_start <= col && col <= col_end) {
			c = '~';
		} else {
			c = ' ';
		}
		if (line[i] == '\t') {
			for (u32 k = 0; k < 8; k++) {
				buf[len++] = c;
			}
			col += 8;
		} else {
			buf[len++] = c;
			col++;
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
	if (state->last_tab) {
		state->location.column += 8;
	} else if (state->last_not_newline) {
		state->location.column++;
	} else {
		state->location.line++;
		state->location.column = 1;
	}
	if (state->index < state->file.len) {
		s32 c = state->file.data[state->index++];
		state->last_not_newline = c != '\n';
		state->last_tab = c == '\t';
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
		ADD_WHILE(c == ' ');
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
					REPORT_ERROR(ERROR_LEX_EOF_IN_COMMENT);
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
	if (is_symbol(c)) {
		token->type = TOK_SYMBOL;
		return;
	}
	if (is_bracket(c)) {
		token->type = TOK_BRACKET;
		return;
	}
	if (is_start_letter(c)) {
		token->type = TOK_WORD;
		ADD_WHILE(is_continue_letter(c));
		return;
	}
	if (c == '"') {
		token->type = TOK_STRING;
		while (true) {
			c = peek(state);
			if (c == -1) {
				REPORT_ERROR(ERROR_LEX_EOF_IN_STRING);
				break;
			}
			if (c == '\n') {
				REPORT_ERROR(ERROR_LEX_UNTERMINATED_STRING);
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

internal
u32 newline_indent(Token *newline) {
	assert(newline->type == TOK_NEWLINE);
	u32 i = 1;
	u32 indent = 0;
	while (i < newline->len) {
		if (newline->start[i++] != '\t') {
			break;
		}
		indent++;
	}
	return indent;
}

internal
u32 newline_align(Token *newline) {
	assert(newline->type == TOK_NEWLINE);
	for (u32 i = 1; i < newline->len; i++) {
		if (newline->start[i] != '\t') {
			return newline->len - i;
		}
	}
	return 0;
}

internal
SourceFile validate_utf8(SourceFile file, u32 lines, ErrorCount *errors) {
	Location location = {};
	SourceFile vfile = {
		file.name,
		malloc(file.len),
		0,
		malloc(lines * sizeof(u32)),
	};
	u32 index = 0;
	b32 last_not_newline = false;
	b32 last_tab = false;
	b32 first_char = true;
#define REPORT_ERROR(code) report_error(errors, &file, code, location, (Range){})
	while (index < file.len) {
		if (last_tab) {
			location.column += 8;
		} else if (last_not_newline) {
			location.column++;
		} else {
			location.line++;
			location.column = 1;
			file.lines[location.line - 1] = index;
			vfile.lines[location.line - 1] = vfile.len;
		}
retry:
		if (index >= file.len) {
			break;
		}
		u32 c = file.data[index++];
		last_not_newline = c != '\n';
		last_tab = c == '\t';
		if (c == 0xff) {
			REPORT_ERROR(ERROR_UTF8_OVERLONG_SEQUENCE);
			while (index < file.len && file.data[index] >> 6 == 2) {
				index++;
			}
			goto retry;
		}
		u32 count = (u32)__builtin_clz((u32)c ^ 0xff) - 24;
		if (count == 1) {
			REPORT_ERROR(ERROR_UTF8_UNEXPECTED_CONTINUATION_CHAR);
			goto retry;
		} else if (count > 4) {
			REPORT_ERROR(ERROR_UTF8_OVERLONG_SEQUENCE);
			while (index < file.len && file.data[index] >> 6 == 2) {
				index++;
			}
			goto retry;
		} else if (count == 0) {
			vfile.data[vfile.len++] = (u8)c;
		} else {
			u8 chars[count];
			chars[0] = (u8)c;
			chars[0] <<= count;
			chars[0] >>= count;
			for (u32 i = 1; i < count; i++) {
				if (index == file.len) {
					REPORT_ERROR(ERROR_UTF8_EOF_IN_SEQUENCE);
					goto retry;
				}
				chars[i] = file.data[index];
				if ((chars[i] >> 6) != 2) {
					REPORT_ERROR(ERROR_UTF8_SEQUENCE_TOO_SHORT);
					goto retry;
				}
				chars[i] &= 0x7f;
				index++;
			}
			c = (u32)chars[0] << (count - 1) * 6;
			for (u32 i = 1; i < count; i++) {
				c |= (u32)chars[i] << (count - i - 1) * 6;
			}
			if (first_char && c == 0xfeff) {
				REPORT_ERROR(ERROR_UTF8_BOM_NOT_ALLOWED);
				goto retry;
			}
			if (c <= 0x7f ||
			    count > 2 && c <= 0x7ff ||
			    count > 3 && c <= 0xffff) {
				REPORT_ERROR(ERROR_UTF8_WRONG_SEQUENCE_LENGTH);
				goto retry;
			}
			if (c >= 0xd800 && c <= 0xdfff) {
				REPORT_ERROR(ERROR_UTF8_SURROGATES_NOT_ALLOWED);
				goto retry;
			}
			if (c > 0x10ffff) {
				REPORT_ERROR(ERROR_UTF8_CODE_POINT_TOO_HIGH);
				goto retry;
			}
			for (u32 i = 0; i < count; i++) {
				vfile.data[vfile.len++] = file.data[index - count + i];
			}
		}
		first_char = false;
	}
	free(file.data);
	free(file.lines);
	vfile.data = realloc(vfile.data, vfile.len);
	return vfile;
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
	ErrorCount errors = {};
	errors.limit = 20;

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
			errors.limit = (u32)value;
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
		// Ensure that we fit within a u32 even if we need to add a final newline
		if (newcap < cap || newcap == -1u) {
			fprintf(stderr, "%s exceeds maximum file size of 4GB - 1\n", file.name);
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
		file.data = realloc(file.data, file.len + 1);
		file.data[file.len++] = '\n';
		lines++;
	} else {
		file.data = realloc(file.data, file.len);
	}
	file.lines = malloc(lines * sizeof(u32));
	file = validate_utf8(file, lines, &errors);
	LexerState state = {};
	state.errors = &errors;
	state.file = file;
	Token token = { .type = TOK_UNKNOWN };
	while (token.type != TOK_EOF) {
		next_token(&state, &token);
		printf("%s:%u:%u: ", file.name, token.location.line, token.location.column);
		switch (token.type) {
		case TOK_WORD: printf("WORD \"%.*s\"", token.len, token.start); break;
		case TOK_SYMBOL: printf("SYMBOL '%.*s'", token.len, token.start); break;
		case TOK_BRACKET: printf("BRACKET '%.*s'", token.len, token.start); break;
		case TOK_STRING: printf("STRING \"%.*s\"", token.len, token.start); break;
		case TOK_NEWLINE: printf("NEWLINE indent=%u, align=%u",
		                         newline_indent(&token), newline_align(&token)); break;
		case TOK_SPACES: printf("SPACES \"%.*s\"", token.len, token.start); break;
		case TOK_COMMENT: printf("COMMENT \"%.*s\"", token.len, token.start); break;
		case TOK_UNKNOWN: printf("UNKNOWN \"%.*s\"", token.len, token.start); break;
		case TOK_EOF: printf("EOF"); break;
		}
		printf("\n");
	}
	if (errors.count > 0) {
		if (!errors.limit || errors.count <= errors.limit) {
			fprintf(stderr, "%u errors\n", errors.count);
		} else {
			fprintf(stderr, "%u errors, only first %u reported\n", errors.count, errors.limit);
		}
		return 1;
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
