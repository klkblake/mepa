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

	ERROR_SYN_EXPECTED,
	ERROR_SYN_EXTRA_CLOSING_BRACKET,
	ERROR_END,

	NOTE_SYN_TO_MATCH,
} ErrorCode;

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

	"hit EOF while looking for end of comment",
	"hit EOF while looking for end of string",
	"string was not terminated before end of line",

	"expected closing '%0'",
	"extraneous closing bracket",
	NULL,

	"to match this '%0'",
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
void vreport_error_line(ErrorCount *errors, char *file, u8 *line, ErrorCode code, Location location, Range range,
                        va_list args) {
	errors->count++;
	if (errors->limit && errors->count > errors->limit) {
		return;
	}
	// TODO only use control codes if output is terminal
	// TODO sort out columns vs characters vs bytes in this function
#define TERM_RED     "\x1b[1;31m"
#define TERM_WHITE   "\x1b[1;37m"
#define TERM_GREEN   "\x1b[1;32m"
#define TERM_GREY    "\x1b[1;30m"
#define TERM_MAGENTA "\x1b[1;35m" /* XXX for warnings, when they are added */
#define TERM_RESET   "\x1b[0m"
	u8 *end = rawmemchr((char *)line, '\n');
	u32 size = (u32) (end - line);
	fprintf(stderr, TERM_WHITE "%s:%d:%d: ", file, location.line, location.column);
	if (code < ERROR_END) {
		fputs(TERM_RED "error: " TERM_WHITE, stderr);
	} else {
		fputs(TERM_GREY "note: " TERM_WHITE, stderr);
	}
	char *message = error_messages[code];
	u32 message_len = (u32)strlen(message);
	u32 argc = 0;
	for (u32 i = 0; i < message_len; i++) {
		if (message[i] == '%') {
			argc++;
		}
	}
	char *argv[argc];
	if (argc) {
		for (u32 i = 0; i < argc; i++) {
			argv[i] = va_arg(args, char *);
		}
	}
	char *message_end = message + message_len;
	while (message < message_end) {
		char *next_spec = strchr(message, '%');
		if (next_spec == NULL) {
			fwrite(message, 1, (u32)(message_end - message), stderr);
			break;
		}
		fwrite(message, 1, (u32)(next_spec - message), stderr);
		message = next_spec;
		assert(message + 1 < message_end);
		u8 c = (u8)message[1];
		assert('0' <= c && c <= '0');
		c -= '0';
		u32 arg_len = (u32)strlen(argv[c]);
		fwrite(argv[c], 1, arg_len, stderr);
		message += 2;
	}
	fputs(TERM_RESET "\n", stderr);
	u32 num_cols = 0;
	for (u32 i = 0; i < size; i++) {
		// TODO Map C1 codes (code points 0x80-0x9f, utf-8 0xc2 0x80 to 0xc2 9f) to the replacement character
		// TODO sanitise against malformed UTF-8
		if (line[i] == '\t') {
			fputs("        ", stderr);
			num_cols += 7;
		} else if (line[i] < ' ') {
			// Map C0 control codes to control pictures
			u8 chr[] = { 0xe2, 0x90, 0x80 | line[i] };
			fwrite(chr, 1, sizeof(chr), stderr);
		} else if (line[i] == 0x7f) {
			// Map DEL to its control picture
			u8 chr[] = { 0xe2, 0x90, 0xa1 };
			fwrite(chr, 1, sizeof(chr), stderr);
		} else {
			fputc(line[i], stderr);
		}
		num_cols++;
	}
	fputc('\n', stderr);
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
	fputs(TERM_GREEN, stderr);
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
				fputc(c, stderr);
			}
			col += 8;
		} else {
			fputc(c, stderr);
			col++;
		}
	}
	fputs(TERM_RESET "\n", stderr);
}

internal
void report_error_line(ErrorCount *errors, char *file, u8 *line, ErrorCode code, Location location, Range range, ...) {
	va_list args;
	va_start(args, range);
	vreport_error_line(errors, file, line, code, location, range, args);
	va_end(args);
}

internal
void report_error(ErrorCount *errors, SourceFile *file, ErrorCode code, Location location, Range range, ...) {
	va_list args;
	va_start(args, range);
	vreport_error_line(errors, file->name, file->data + file->lines[location.line - 1], code, location, range,
	                   args);
	va_end(args);
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
	u32 line_offset = 0;
	u32 index = 0;
	b32 last_not_newline = false;
	b32 last_tab = false;
	b32 first_char = true;
#define REPORT_ERROR(code) report_error_line(errors, file.name, file.data + line_offset, code, location, (Range){})
	while (index < file.len) {
		if (last_tab) {
			location.column += 8;
		} else if (last_not_newline) {
			location.column++;
		} else {
			location.line++;
			location.column = 1;
			line_offset = index;
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
#undef REPORT_ERROR
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

typedef struct {
	u8 *data;
	u32 len;
	u32 cap;
} Buf;

internal
void buf_append(Buf *buf, u8 *src, u32 len) {
	if (buf->len + len > buf->cap) {
		buf->cap += buf->cap >> 1;
		buf->data = realloc(buf->data, buf->cap);
	}
	memcpy(buf->data + buf->len, src, len);
	buf->len += len;
}

internal
void buf_push(Buf *buf, u8 c) {
	if (buf->len >= buf->cap) {
		buf->cap += buf->cap >> 1;
		buf->data = realloc(buf->data, buf->cap);
	}
	buf->data[buf->len++] = c;
}

internal
void print_newline(Buf *buf, Token *token, u32 wanted_indent, u32 wanted_align) {
	u32 given_indent = newline_indent(token);
	u32 given_align = newline_align(token);
	if (given_indent == wanted_indent && given_align == wanted_align) {
		buf_append(buf, token->start, token->len);
	} else {
		buf_push(buf, '\n');
		for (u32 i = 0; i < wanted_indent; i++) {
			buf_push(buf, '\t');
		}
		for (u32 i = 0; i < wanted_align; i++) {
			buf_push(buf, ' ');
		}
	}
}

internal
int process_common_command_line(int argc, char *argv[static argc], SourceFile *vfile, ErrorCount *errors) {
	SourceFile file = {};
	*errors = (ErrorCount){};
	errors->limit = 20;

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
			errors->limit = (u32)value;
			break;
		}
		case '?': return EX_USAGE;
		}
	}
	if (argc - optind > 1) {
		fprintf(stderr, "format expects exactly one file");
		return EX_USAGE;
	}
	FILE *file_stream;
	if (optind == argc || strcmp(argv[optind], "-") == 0) {
		file_stream = stdin;
		file.name = "<stdin>";
	} else {
		file_stream = nonnull_or_die(fopen(argv[optind], "r"), EX_NOINPUT);
		file.name = argv[optind];
	}
	s64 estimate = -1;
	if (fseek(file_stream, 0, SEEK_END) == 0) {
		estimate = ftell(file_stream);
		if (estimate != -1) {
			if (estimate >= -1u) {
				fprintf(stderr, "%s exceeds maximum file size of 4GB - 1\n", file.name);
				return EX_DATAERR;
			}
		}
		rewind(file_stream);
	}
	u32 cap;
	if (estimate != -1) {
		cap = estimate > 8 ? (u32)estimate : 8;
	} else {
		cap = 4096;
	}
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
	if (estimate != -1 && file.len != estimate) {
		fprintf(stderr, "%s changed whilst reading", file.name);
		return EX_IOERR;
	}
	if (file.data[file.len - 1] != '\n') {
		file.data = realloc(file.data, file.len + 1);
		file.data[file.len++] = '\n';
		lines++;
	} else {
		file.data = realloc(file.data, file.len);
	}
	*vfile = validate_utf8(file, lines, errors);
	return 0;
}

internal
b32 print_error_summary(ErrorCount errors) {
	if (errors.count > 0) {
		if (!errors.limit || errors.count <= errors.limit) {
			fprintf(stderr, "%u errors\n", errors.count);
		} else {
			fprintf(stderr, "%u errors, only first %u reported\n", errors.count, errors.limit);
		}
		return true;
	}
	return false;
}

internal
int format_main(int argc, char *argv[static argc]) {
	SourceFile file;
	ErrorCount errors;
	process_common_command_line(argc, argv, &file, &errors);

	LexerState state = {};
	state.errors = &errors;
	state.file = file;
	typedef struct {
		u32 indent;
		u32 align;
		u8 type;
		Location location;
	} Indent;
	struct {
		Indent *data;
		u32 len;
		u32 cap;
	} indent_stack;
	indent_stack.cap = 8;
	indent_stack.data = malloc(indent_stack.cap * sizeof(Indent));
	indent_stack.len = 1;
	indent_stack.data[0] = (Indent){};
	Token prev_token = { .type = TOK_UNKNOWN };
	Token token;
#define REPORT_ERROR(code, location, ...) report_error(&errors, &file, code, location, (Range){}, ##__VA_ARGS__)
	u32 post_indent_column = 1;
	Buf output = {};
	output.cap = file.len;
	output.data = malloc(output.cap);
	while (token.type != TOK_EOF) {
		next_token(&state, &token);
		switch (token.type) {
		// TODO enforce open brace not on new line. Maybe compress multiple newline tokens?
		case TOK_BRACKET:
		{
			u8 c = *token.start;
			// Map to a dense range
			// '(', ')' => 0
			// '[', ']' => 1
			// '{', '}' => 2
			u8 type = (c >> 5) - 1;
			u8 open[]  = { '(', '[', '{' };
			u8 close[] = { ')', ']', '}' };
			if (c == open[type]) {
				if (indent_stack.len == indent_stack.cap) {
					indent_stack.cap += indent_stack.cap >> 1;
					indent_stack.data = realloc(indent_stack.data,
					                            indent_stack.cap * sizeof(Indent));
				}
				Indent *old = &indent_stack.data[indent_stack.len - 1];
				Indent *new = &indent_stack.data[indent_stack.len++];
				new->location = token.location;
				new->type = type;
				if (c == '{') {
					new->indent = old->indent + 1;
					new->align = 0;
				} else {
					new->indent = old->indent;
					new->align = post_indent_column;
				}
				if (prev_token.type == TOK_NEWLINE) {
					print_newline(&output, &prev_token, old->indent, old->align);
					post_indent_column = 1 + old->align;
				}
			} else {
				u32 match = indent_stack.len - 1;
				while (match > 0 && indent_stack.data[match].type != type) {
					match--;
				}
				Indent *indent;
				if (indent_stack.len == 1) {
					REPORT_ERROR(ERROR_SYN_EXTRA_CLOSING_BRACKET, token.location);
					indent = &indent_stack.data[0];
				} else if (match == 0) {
					REPORT_ERROR(ERROR_SYN_EXTRA_CLOSING_BRACKET, token.location);
					indent = &indent_stack.data[indent_stack.len - 1];
				} else if (match < indent_stack.len - 1) {
					u8 wanted = indent_stack.data[indent_stack.len - 1].type;
					u8 open_str[2] = { open[wanted], 0 };
					u8 close_str[2] = { close[wanted], 0 };
					REPORT_ERROR(ERROR_SYN_EXPECTED, token.location, close_str);
					REPORT_ERROR(NOTE_SYN_TO_MATCH,
					             indent_stack.data[indent_stack.len - 1].location, open_str);
					indent_stack.len = match;
					indent = &indent_stack.data[match];
					if (type == 2) {
						indent->indent--;
					}
				} else {
					indent_stack.len--;
					indent = &indent_stack.data[indent_stack.len];
					if (type == 2) {
						indent->indent--;
					}
				}
				if (prev_token.type == TOK_NEWLINE) {
					print_newline(&output, &prev_token, indent->indent, indent->align);
					post_indent_column = 1 + indent->align;
				}
			}
			assert(token.len == 1);
			buf_append(&output, token.start, 1);
			post_indent_column++;
			break;
		}
		case TOK_WORD:
		case TOK_SYMBOL:
		case TOK_STRING:
		case TOK_SPACES:
		case TOK_COMMENT:
		case TOK_UNKNOWN:
		{
			if (prev_token.type == TOK_NEWLINE) {
				Indent *indent = &indent_stack.data[indent_stack.len - 1];
				print_newline(&output, &prev_token, indent->indent, indent->align);
				post_indent_column = 1 + indent->align;
			}
			buf_append(&output, token.start, token.len);
			// TODO column vs character vs byte count
			post_indent_column += token.len;
			break;
		}
		case TOK_NEWLINE:
		case TOK_EOF:
		{
			if (prev_token.type == TOK_NEWLINE) {
				buf_push(&output, '\n');
				post_indent_column = 1;
			}
			break;
		}
		}
		prev_token = token;
	}
#undef REPORT_ERROR
	// TODO don't free memory that is about to be freed by program exit
	free(indent_stack.data);
	if (print_error_summary(errors)) {
		fwrite(output.data, 1, output.len, stdout);
		return 1;
	}
	fwrite(output.data, 1, output.len, stdout);
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
	setlinebuf(stderr);
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
