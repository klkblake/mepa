#include "unicode.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>
#include <errno.h>
#include <sysexits.h>

typedef struct {
	u32 line;
	u32 column;
} Location;

typedef struct Bracket {
	u32 offset;
	u8 c;
} Bracket;

typedef struct {
	u32 offset;
	u32 indent;
	u32 align;
} Line;

typedef struct {
	char *name;
	u8 *data;
	u32 len;
	u32 token_count;
	u32 *token_offsets;
	u32 line_count;
	Line *lines;
	u32 bracket_count;
	Bracket *brackets;
} SourceFile;

typedef struct {
	b32 use_color;
	b32 fatal;
	u32 count;
	u32 limit;
} ErrorCount;

internal
Location location_for_offset(SourceFile *file, u32 offset) {
	if (offset == -1u) {
		return (Location){};
	}
	u32 lo = 0;
	u32 hi = file->line_count - 1;
	while (lo < hi) {
		u32 mid = (lo + hi) / 2 + 1;
		if (file->lines[mid].offset > offset) {
			hi = mid - 1;
		} else {
			lo = mid;
		}
	}
	return (Location){ lo + 1, offset - file->lines[lo].offset + 1 };
}

#define ERROR "E"
#define NOTE "N"
internal
void vreport_error_line(ErrorCount *errors, char *file, u8 *line, char *message, Location location,
                        Location range_start, Location range_end, va_list args) {
	if (message[0] != 'N' && errors->count < -1u) {
		errors->count++;
		if (errors->limit && errors->count == errors->limit) {
			errors->fatal = true;
		}
	}
	if (errors->limit && errors->count > errors->limit) {
		return;
	}
	// TODO sort out columns vs characters vs bytes in this function
	const char *term_red = "";
	const char *term_white = "";
	const char *term_green = "";
	const char *term_grey = "";
	const char *term_magenta = ""; // XXX for warnings, when they are added
	const char *term_reset = "";
	if (errors->use_color) {
		term_red = "\x1b[1;31m";
		term_white = "\x1b[1;37m";
		term_green = "\x1b[1;32m";
		term_grey = "\x1b[1;30m";
		term_magenta = "\x1b[1;35m";
		term_reset = "\x1b[0m";
	}
	fprintf(stderr, "%s%s:%d:%d: ", term_white, file, location.line, location.column);
	if (message[0] == 'E') {
		fprintf(stderr, "%serror:%s ", term_red, term_white);
	} else {
		assert(message[0] == 'N');
		fprintf(stderr, "%snote:%s ", term_grey, term_white);
	}
	message++;
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
		assert('0' <= c && c <= '9');
		c -= '0';
		u32 arg_len = (u32)strlen(argv[c]);
		fwrite(argv[c], 1, arg_len, stderr);
		message += 2;
	}
	fprintf(stderr, "%s\n", term_reset);
	if (!line) {
		return;
	}
	u8 *end = rawmemchr((char *)line, '\n');
	u32 size = (u32) (end - line);
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
	assert(location.line || !range_start.line && !range_end.line);
	assert(!range_start.line || range_start.line <= location.line);
	assert(!range_end.line || range_end.line >= location.line);
	u32 col_start = 0;
	if (range_start.line == 0) {
		col_start = -1u;
	} else if (range_start.line == location.line) {
		col_start = range_start.column - 1;
	}
	u32 col_end = num_cols - 1;
	if (range_end.line == location.line) {
		col_end = range_end.column - 1;
	}
	fputs(term_green, stderr);
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
				fputc(c, stderr);
			}
		} else {
			fputc(c, stderr);
		}
	}
	fprintf(stderr, "%s\n", term_reset);
}

internal
void vreport_error(ErrorCount *errors, SourceFile *file, char *message, Location location,
                   Location range_start, Location range_end, va_list args) {
	if (location.line) {
		vreport_error_line(errors, file->name, file->data + file->lines[location.line - 1].offset, message,
		                   location, range_start, range_end, args);
	} else {
		vreport_error_line(errors, file->name, NULL, message, location, range_start, range_end, args);
	}
}

typedef struct {
	SourceFile file;
	u32 line_offset;
	ErrorCount *errors;
} UTF8Validator;

internal
void report_utf8_error(UTF8Validator *state, u32 offset, char *message, ...) {
	va_list args;
	va_start(args, message);
	Location none = {};
	vreport_error_line(state->errors, state->file.name, state->file.data + state->line_offset, message,
	                   location_for_offset(&state->file, offset), none, none, args);
	va_end(args);
}

// TODO unicode normalisation
internal
SourceFile validate_utf8(SourceFile file, ErrorCount *errors) {
	SourceFile vfile = file;
	vfile.data = malloc(file.len);
	vfile.len = 0;
	vfile.lines[0] = (Line){};
	UTF8Validator state = {};
	state.file = file;
	state.errors = errors;
	u32 index = 0;
	u32 line = 0;
	b32 first_char = true;
	while (index < file.len) {
		u32 c = file.data[index++];
		if (c == 0) {
			report_utf8_error(&state, vfile.len, ERROR "illegal NUL byte");
			continue;
		}
		if (c == '\n') {
			vfile.lines[++line] = (Line){vfile.len + 1, 0, 0};
		}
		u32 count;
		if (c == 0xff) {
			count = 5;
		} else {
			count = (u32)__builtin_clz((u32)c ^ 0xff) - 24;
		}
		if (count == 1) {
			report_utf8_error(&state, vfile.len, ERROR "unexpected continuation byte");
			continue;
		} else if (count > 4) {
			report_utf8_error(&state, vfile.len, ERROR "sequence too long (> 4 bytes)");
			while (index < file.len && file.data[index] >> 6 == 2) {
				index++;
			}
			continue;
		} else if (count == 0) {
			vfile.data[vfile.len++] = (u8)c;
		} else {
			u8 chars[count];
			chars[0] = (u8)c;
			chars[0] <<= count;
			chars[0] >>= count;
			for (u32 i = 1; i < count; i++) {
				if (index == file.len) {
					report_utf8_error(&state, vfile.len, ERROR "hit EOF while decoding sequence");
					continue;
				}
				chars[i] = file.data[index];
				if ((chars[i] >> 6) != 2) {
					report_utf8_error(&state, vfile.len,
					                  ERROR "too few continuation bytes in sequence");
					continue;
				}
				chars[i] &= 0x7f;
				index++;
			}
			c = (u32)chars[0] << (count - 1) * 6;
			for (u32 i = 1; i < count; i++) {
				c |= (u32)chars[i] << (count - i - 1) * 6;
			}
			if (first_char && c == 0xfeff) {
				report_utf8_error(&state, vfile.len, ERROR "byte order markers are not permitted");
				continue;
			}
			if (c <= 0x7f ||
			    count > 2 && c <= 0x7ff ||
			    count > 3 && c <= 0xffff) {
				report_utf8_error(&state, vfile.len, ERROR "sequence length too long for code point");
				continue;
			}
			if (c >= 0xd800 && c <= 0xdfff) {
				report_utf8_error(&state, vfile.len, ERROR "surrogates are not permitted");
				continue;
			}
			if (c > 0x10ffff) {
				report_utf8_error(&state, vfile.len, ERROR "code point exceeded limit of 0x10ffff");
				continue;
			}
			for (u32 i = 0; i < count; i++) {
				vfile.data[vfile.len++] = file.data[index - count + i];
			}
		}
		first_char = false;
	}
	free(file.data);
	vfile.data = realloc(vfile.data, vfile.len);
	return vfile;
}

typedef struct {
	s32 cp;
	u32 len;
} UTF8Codepoint;

internal
UTF8Codepoint decode_utf8_codepoint(u8 *buf, u32 index, u32 len) {
	if (index == len) {
		return (UTF8Codepoint){-1, 0};
	}
	UTF8Codepoint c;
	c.cp = buf[index];
	c.len = (u32)__builtin_clz((u32)c.cp ^ 0xff) - 24;
	if (c.len > 0) {
		c.cp &= 0xff >> c.len;
		for (u32 i = 1; i < c.len; i++) {
			c.cp <<= 6;
			c.cp |= buf[index + i] & 0x7f;
		}
	} else {
		c.len = 1;
	}
	return c;
}

internal
u32 utf8_codepoint_length(u8 *buf, u32 index, u32 len) {
	if (index == len) {
		return 0;
	}
	s32 c = buf[index];
	u32 seq_len = (u32)__builtin_clz((u32)c ^ 0xff) - 24;
	if (seq_len > 0) {
		return seq_len;
	}
	return 1;
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
	return c == '_' || in_unicode_range((u32)c,
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

// Brackets are mapped to types as follows:
// '(', ')' => 0
// '[', ']' => 1
// '{', '}' => 2
#define BRACKET_TYPE(c) ((c >> 5) - 1)
internal u8 open_bracket[]  = { '(', '[', '{' };
internal u8 close_bracket[] = { ')', ']', '}' };

internal
void report_tokenise_error(ErrorCount *errors, SourceFile *file, u32 token_start, u32 token_end, char *message, ...) {
	va_list args;
	va_start(args, message);
	Location start_location = location_for_offset(file, token_start);
	Location end_location = location_for_offset(file, token_end);
	vreport_error(errors, file, message, start_location, start_location, end_location, args);
	va_end(args);
}

// TODO first pass analyses brackets based on formatting to force them to all match up.
// TODO second pass does a shift-reduce parse with error productions e.g. '(' error ')'
internal
SourceFile tokenise(SourceFile file, ErrorCount *errors) {
#define peek() decode_utf8_codepoint(file.data, index, file.len)
#define accept() (index += c.len, c.cp = 0, c.len = utf8_codepoint_length(file.data, index, file.len))
#define next() (index += c.len, peek())
#define NEXT_TOKEN suppress_illegal_characters = false; continue
	b32 suppress_illegal_characters = false;
	u32 index = 0;
	u32 line_index = 0;
	u32 bracket_index = 0;
	file.token_offsets = malloc((file.len + 1) * sizeof(u32));
	file.token_offsets[0] = 0;
	UTF8Codepoint c = peek();
	if (c.cp == '\t' || c.cp == ' ') {
		file.token_offsets++;
		Line *line = &file.lines[0];
		while (c.cp == '\t') {
			line->indent++;
			c = next();
		}
		while (c.cp == ' ') {
			line->align++;
			c = next();
		}
	}
	while (true) {
		u32 start = index;
		file.token_offsets[file.token_count++] = start;
		if (c.cp == -1) {
			break;
		}
		if (c.cp == '\n') {
			Line *line = &file.lines[++line_index];
			c = next();
			while (c.cp == '\t') {
				line->indent++;
				c = next();
			}
			while (c.cp == ' ') {
				line->align++;
				c = next();
			}
			NEXT_TOKEN;
		}
		if (c.cp == ' ') {
			c = next();
			while (c.cp == ' ') {
				c = next();
			}
			NEXT_TOKEN;
		}
		if (c.cp == '"') {
			while (true) {
				c = next();
				if (c.cp == -1) {
					report_tokenise_error(errors, &file, start, index,
					                      ERROR "hit EOF while looking for end of string");
					break;
				}
				if (c.cp == '\n') {
					report_tokenise_error(errors, &file, start, index,
					                      ERROR "string was not terminated before end of line");
					break;
				}
				if (c.cp == '"') {
					c = next();
					break;
				}
				if (c.cp == '\\') {
					accept();
				}
			}
			NEXT_TOKEN;
		}
		if (c.cp == '/') {
			c = next();
			if (c.cp == '/') {
				c = next();
				while (c.cp != '\n' && c.cp != -1) {
					c = next();
				}
				NEXT_TOKEN;
			}
			if (c.cp == '*') {
				u32 depth = 1;
				while (depth > 0) {
					c = next();
					if (c.cp == -1) {
						report_tokenise_error(errors, &file, start, index,
						                      ERROR "hit EOF while looking for end of comment");
						break;
					}
					if (c.cp == '/') {
						c = next();
						if (c.cp == '*') {
							depth++;
						}
						c = next();
					} else if (c.cp == '*') {
						c = next();
						if (c.cp == '/') {
							depth--;
						}
						c = next();
					}
				}
				NEXT_TOKEN;
			}
			// we aren't actually processing a comment
			while (is_symbol(c.cp)) {
				c = next();
			}
			NEXT_TOKEN;
		}
		if (is_bracket(c.cp)) {
			file.brackets[bracket_index++] = (Bracket){start, (u8)c.cp};
			c = next();
			NEXT_TOKEN;
		}
		if ('0' <= c.cp && c.cp <= '9' || is_start_letter(c.cp)) {
			c = next();
			while (is_continue_letter(c.cp)) {
				c = next();
			}
			NEXT_TOKEN;
		}
		if (is_symbol(c.cp)) {
			c = next();
			while (is_symbol(c.cp)) {
				c = next();
			}
			NEXT_TOKEN;
		}
		if (!suppress_illegal_characters) {
			report_tokenise_error(errors, &file, start, index,
			                      ERROR "illegal character");
			suppress_illegal_characters = true;
		}
		c = next();
	}
	file.token_offsets = realloc(file.token_offsets, file.token_count * sizeof(u32));
	file.bracket_count = bracket_index;
	file.brackets = realloc(file.brackets, file.bracket_count * sizeof(Bracket));
	return file;
#undef NEXT_TOKEN
#undef next
#undef accept
#undef peek
}

internal
void report_bracket_error(ErrorCount *errors, SourceFile *file, u32 offset, char *message, ...) {
	va_list args;
	va_start(args, message);
	Location none = {};
	vreport_error(errors, file, message, location_for_offset(file, offset), none, none, args);
	va_end(args);
}

internal
void balance_brackets(SourceFile file, ErrorCount *errors) {
	u32 bracket_count = 0;
	u32 bracket_cap = 8;
	Bracket **brackets = malloc(bracket_cap * sizeof(Bracket *));
	// TODO optionally use formatting information
	// TODO use better correction algorithm
	for (u32 index = 0; index < file.bracket_count; index++) {
		Bracket *bracket = &file.brackets[index];
		u8 type = BRACKET_TYPE(bracket->c);
		if (bracket->c == open_bracket[type]) {
			if (bracket_count == bracket_cap) {
				bracket_cap += bracket_cap >> 1;
				brackets = realloc(brackets, bracket_cap * sizeof(Bracket *));
			}
			brackets[bracket_count++] = bracket;
		} else {
			s32 match = (s32)bracket_count - 1;
			while (match >= 0 && BRACKET_TYPE(brackets[match]->c) != type) {
				match--;
			}
			if (match == -1 || bracket_count == 0) {
				report_bracket_error(errors, &file, bracket->offset,
				                     ERROR "extraneous closing bracket");
				bracket->c = 0;
			} else if ((u32)match < bracket_count - 1) {
				Bracket *top = brackets[bracket_count - 1];
				u8 wanted = BRACKET_TYPE(top->c);
				u8 open_str[] = { open_bracket[wanted], 0 };
				u8 close_str[] = { close_bracket[wanted], 0 };
				report_bracket_error(errors, &file, bracket->offset,
				                     ERROR "expected closing '%0'", close_str);
				report_bracket_error(errors, &file, top->offset,
				                     NOTE "to match this '%0'", open_str);
				if ((u32)match + 1 != bracket_count - 1) {
					for (u32 i = bracket_count - 1; i > (u32)match; i--) {
						u8 bstr[] = { brackets[i]->c, 0 };
						report_bracket_error(errors, &file, brackets[i]->offset,
						                     NOTE "deleting unclosed bracket '%0'", bstr);
						brackets[i]->c = 0;
					}
				} else {
					brackets[match + 1]->c = 0;
				}
				bracket_count = (u32)match;
			} else {
				bracket_count--;
			}
		}
	}
	free(brackets);
}

#define NONTERM_BIT    (1u << 15)
#define TOK_NONE       ((u16)-1u)
#define TOK_EOF        0
#define TOK_NEWLINE    1
#define TOK_STRING     2
#define TOK_NUMBER     3
#define TOK_BRACKETS_START 4
#define TOK_LPAREN     4
#define TOK_RPAREN     5
#define TOK_LBRACKET   6
#define TOK_RBRACKET   7
#define TOK_LBRACE     8
#define TOK_RBRACE     9
#define TOK_BRACKETS_END 9
#define TOK_IDENT      10
#define TOK_EXTRA_BASE 11

internal
char *token_strings[] = {
	"<EOF>",
	"<newline>",
	"<string>",
	"<number>",
	"'('",
	"')'",
	"'['",
	"']'",
	"'{'",
	"'}'",
	"<ident>",
};

// Nonterminal if NONTERM_BIT is set, terminal otherwise
typedef struct {
	u16 first; // TODO check for overflow when adding rules
	u16 second;
	u32 offset_start;
	u32 offset_end;
} Rule;

typedef struct {
	char *nonterminal;
	u32 next;
	u32 count; // only used in first block
	Rule rules[4];
} RuleSet;

typedef struct {
	u32 next;
	Rule rules[5];
} ExtraRules;

// The parser table consists of one entry for each rule set. Each entry is a
// record count followed by a series of records. Each record corresponds to a
// rule, and is composed of the set of terminals that trigger that rule. The
// last terminal is marked with END_BIT.
#define END_BIT (1u << 15)

#define BITSET_WORD_SIZE (sizeof(u64) * 8)

typedef struct {
	SourceFile *file;
	ErrorCount *errors;

	u32 extra_token_count;
	char **extra_tokens;
	char **extra_token_strings;
	u32 ruleset_count;
	RuleSet *rulesets;
	u32 extra_block_count;
	ExtraRules *extra_rules;
	u32 bitset_width;
	u64 *first_set_table;
	u32 *parse_table_index;
	u16 *parse_table;
} Parser;
#define FIRST_SET_TABLE(parser) ((u64 (*)[(parser)->bitset_width])(parser)->first_set_table)

internal
char *token_to_string(u32 token, char *extra_token_strings[]) {
	if (token < TOK_EXTRA_BASE) {
		return token_strings[token];
	} else {
		return extra_token_strings[token - TOK_EXTRA_BASE];
	}
}

internal
char *symbol_to_string(Parser *parser, u32 symbol) {
	if ((symbol & NONTERM_BIT) != 0) {
		symbol &= ~NONTERM_BIT;
		return parser->rulesets[symbol].nonterminal;
	} else {
		return token_to_string(symbol, parser->extra_token_strings);
	}
}


typedef struct {
	u32 index;
	u32 count;
	Rule *rule;
	Rule *end;
	u32 next;
} RuleSetIter;

internal
RuleSetIter ruleset_iter_init(RuleSet *ruleset) {
	RuleSetIter iter = {
		0,
		ruleset->count,
		ruleset->rules,
		ruleset->rules + array_count(ruleset->rules),
		ruleset->next,
	};
	return iter;
}

internal
b32 ruleset_iter_valid(RuleSetIter iter) {
	return iter.index < iter.count;
}

internal
RuleSetIter ruleset_iter_next(Parser *parser, RuleSetIter iter) {
	iter.index++;
	if (iter.index >= iter.count) {
		return iter;
	}
	iter.rule++;
	if (iter.rule == iter.end) {
		ExtraRules *extra = &parser->extra_rules[iter.next];
		iter.next = extra->next;
		iter.rule = extra->rules;
		iter.end = iter.rule + array_count(extra->rules);
	}
	return iter;
}
#define foreach_rule(parser, ruleset, iter) \
	for (RuleSetIter iter = ruleset_iter_init(ruleset); \
	     ruleset_iter_valid(iter); \
	     iter = ruleset_iter_next(parser, iter))

internal
void print_parse_rules(Parser *parser) {
	fprintf(stderr, "Parse rules:\n");
	for (u32 i = 0; i < parser->ruleset_count; i++) {
		fprintf(stderr, "Rule set %u %s:\n", i, parser->rulesets[i].nonterminal);
		foreach_rule (parser, &parser->rulesets[i], iter) {
			fprintf(stderr, "  Rule %u: ", iter.index);
			fprintf(stderr, "%s", symbol_to_string(parser, iter.rule->first));
			if (iter.rule->second != TOK_NONE) {
				fprintf(stderr, ", %s", symbol_to_string(parser, iter.rule->second));
			}
			fprintf(stderr, "\n");
		}
		fprintf(stderr, "\n");
	}
	fprintf(stderr, "\n");
}

internal
void print_first_set_table(Parser *parser) {
	fprintf(stderr, "First sets:\n");
	for (u32 i = 0; i < parser->ruleset_count; i++) {
		fprintf(stderr, "Rule set %u %s: ", i, parser->rulesets[i].nonterminal);
		u32 token = 0;
		b32 first = true;
		for (u32 j = 0; j < parser->bitset_width; j++) {
			u64 word = FIRST_SET_TABLE(parser)[i][j];
			for (u32 k = 0; k < BITSET_WORD_SIZE; k++, token++) {
				if (word >> k & 0x1) {
					if (first) {
						first = false;
					} else {
						fprintf(stderr, ", ");
					}
					fprintf(stderr, "%s", token_to_string(token, parser->extra_token_strings));
				}
			}
		}
		fprintf(stderr, "\n");
	}
	fprintf(stderr, "\n\n");
}

internal
void print_parse_table(Parser *parser, u16 *table) {
	fprintf(stderr, "Parse table:\n");
	for (u32 i = 0; i < parser->ruleset_count; i++) {
		fprintf(stderr, "Rule set %u %s:\n", i, parser->rulesets[i].nonterminal);
		for (u32 j = 0; j < parser->rulesets[i].count; j++) {
			fprintf(stderr, "  Rule %u: ", j);
			while (true) {
				u32 word = *table++;
				b32 is_last = word & END_BIT;
				u32 term = word &~ END_BIT;
				fprintf(stderr, "%s", token_to_string(term, parser->extra_token_strings));
				if (is_last) {
					break;
				}
				fprintf(stderr, ", ");
			}
			fprintf(stderr, "\n");
		}
		fprintf(stderr, "\n");
	}
	fprintf(stderr, "\n");
}

internal
void report_parse_error(Parser *parser, u32 offset_start, u32 offset_end, char *message, ...) {
	va_list args;
	va_start(args, message);
	Location start = location_for_offset(parser->file, offset_start);
	Location end = location_for_offset(parser->file, offset_end);
	vreport_error(parser->errors, parser->file, message, start, start, end, args);
	va_end(args);
}

internal
void report_parser_conflict(Parser *parser, RuleSet *ruleset, Rule conflict_rule,
                            u32 conflict_word, u32 conflict_index) {
	parser->errors->fatal = true;
	Rule existing_rule;
	foreach_rule (parser, ruleset, iter) {
		u32 first = iter.rule->first;
		if ((first & NONTERM_BIT) != 0) {
			first &= ~NONTERM_BIT;
			if ((FIRST_SET_TABLE(parser)[first][conflict_word] & 1 << conflict_index) != 0) {
				existing_rule = *iter.rule;
				break;
			}
		} else {
			u32 word = first / BITSET_WORD_SIZE;
			u32 index = first - word * BITSET_WORD_SIZE;
			if (word == conflict_word && index == conflict_index) {
				existing_rule = *iter.rule;
				break;
			}
		}
	}
	char *conflict_first = symbol_to_string(parser, conflict_rule.first);
	char *conflict_comma = "";
	char *conflict_second = "";
	if (conflict_rule.second != TOK_NONE) {
		conflict_comma = ", ";
		conflict_second = symbol_to_string(parser, conflict_rule.second);
	}
	char *existing_first = symbol_to_string(parser, existing_rule.first);
	char *existing_comma = "";
	char *existing_second = "";
	if (existing_rule.second != TOK_NONE) {
		existing_comma = ", ";
		existing_second = symbol_to_string(parser, existing_rule.second);
	}
	char *terminal = token_to_string(conflict_word * BITSET_WORD_SIZE + conflict_index,
	                                 parser->extra_token_strings);
	report_parse_error(parser, -1u, -1u,
	                   ERROR "parse rule conflict on terminal %0", terminal);
	report_parse_error(parser, conflict_rule.offset_start, conflict_rule.offset_end,
	                   NOTE "new parse rule: %0 -> %1%2%3",
	                   ruleset->nonterminal, conflict_first, conflict_comma, conflict_second);
	report_parse_error(parser, existing_rule.offset_start, existing_rule.offset_end,
	                   NOTE "existing parse rule: %0 -> %1%2%3",
	                   ruleset->nonterminal, existing_first, existing_comma, existing_second);
}

internal
u32 bitset_popcount(u32 width, u64 bitset[static width]) {
	u32 count = 0;
	for (u32 i = 0; i < width; i++) {
		count += (u32)__builtin_popcountl(bitset[i]);
	}
	return count;
}

// TODO incremental updates
// TODO proper memory management of intermediate structures
internal
void regen_parse_table(Parser *parser) {
	u32 bitset_width = (TOK_EXTRA_BASE + parser->extra_token_count - 1) / BITSET_WORD_SIZE + 1;
	u64 (*first_set_table)[bitset_width] = calloc(parser->ruleset_count, bitset_width * sizeof(u64));
	u64 (*first_set_table_old)[bitset_width] = calloc(parser->ruleset_count, bitset_width * sizeof(u64));
	parser->bitset_width = bitset_width;
	parser->first_set_table = (u64 *)first_set_table;
	b32 changed;
	do {
		for (u32 i = 0; i < parser->ruleset_count; i++) {
			foreach_rule(parser, &parser->rulesets[i], iter) {
				u32 first = iter.rule->first;
				if ((first & NONTERM_BIT) != 0) {
					first &= ~NONTERM_BIT;
					for (u32 j = 0; j < bitset_width; j++) {
						first_set_table[i][j] |= first_set_table[first][j];
					}
				} else {
					u32 word = first / BITSET_WORD_SIZE;
					u32 index = first - word * BITSET_WORD_SIZE;
					first_set_table[i][word] |= 1 << index;
				}
			}
		}
		changed = memcmp(first_set_table, first_set_table_old, parser->ruleset_count * bitset_width) != 0;
		if (changed) {
			memcpy(first_set_table_old, first_set_table, parser->ruleset_count * bitset_width);
		}
	} while (changed);
	print_first_set_table(parser);
	u32 parse_table_size = 0;
	for (u32 i = 0; i < parser->ruleset_count; i++) {
		parse_table_size += bitset_popcount(bitset_width, first_set_table[i]);
	}
	u16 *parse_table = malloc(parse_table_size * sizeof(u16));
	u64 *used_first_set = malloc(bitset_width * sizeof(u64));
	u32 cursor = 0;
	for (u32 i = 0; i < parser->ruleset_count; i++) {
		RuleSet *rs = &parser->rulesets[i];
		parser->parse_table_index[i] = cursor;
		memset(used_first_set, 0, bitset_width * sizeof(u64));
		foreach_rule(parser, rs, iter) {
			u32 first = iter.rule->first;
			if ((first & NONTERM_BIT) != 0) {
				first &= ~NONTERM_BIT;
				for (u32 j = 0; j < bitset_width; j++) {
					u64 conflict = used_first_set[j] & first_set_table[first][j];
					if (conflict != 0) {
						u32 index = (u32)__builtin_ffsl((s64)conflict) - 1;
						report_parser_conflict(parser, rs, *iter.rule, j, index);
						return;
					}
					used_first_set[j] |= first_set_table[first][j];
				}
			} else {
				u32 word = first / BITSET_WORD_SIZE;
				u32 index = first - word * BITSET_WORD_SIZE;
				u64 conflict = used_first_set[word] & 1 << index;
				if (conflict != 0) {
					report_parser_conflict(parser, rs, *iter.rule, word, index);
					return;
				}
				used_first_set[word] |= 1 << index;
			}
		}
		foreach_rule(parser, rs, iter) {
			u32 first = iter.rule->first;
			if ((first & NONTERM_BIT) != 0) {
				first &= ~NONTERM_BIT;
				for (u32 j = 0; j < bitset_width; j++) {
					u64 word = first_set_table[first][j];
					while (word != 0) {
						u32 next_token_offset = (u32)__builtin_ffsl((s64)word) - 1;
						word >>= next_token_offset + 1;
						word <<= next_token_offset + 1;
						parse_table[cursor++] = (u16)(j * BITSET_WORD_SIZE + next_token_offset);
					}
				}
			} else {
				parse_table[cursor++] = (u16)first;
			}
			parse_table[cursor - 1] |= END_BIT;
		}
	}
	print_parse_table(parser, parse_table);
	parser->parse_table = parse_table;
	free(used_first_set);
	free(first_set_table);
}

typedef struct {
	u16 *data;
	u32 len;
	u32 cap;
} Stack;

internal
void stack_push(Stack *stack, u16 sym) {
	if (stack->len >= stack->cap) {
		stack->cap += stack->cap >> 1;
		stack->data = realloc(stack->data, stack->cap);
	}
	stack->data[stack->len++] = sym;
}

internal
void *memdup(void *src, u64 size) {
	return memcpy(malloc(size), src, size);
}

internal
void parse(SourceFile file, ErrorCount *errors) {
	// TODO detect leading indent
	// TODO error productions
	// TODO parse actions
	char *extra_tokens[] = {
		"__register_keyword",
		"__register_rule_native_hex",
	};
	char *extra_token_strings[] = {
		"\"__register_keyword\"",
		"\"__register_rule_native_hex\"",
	};
	u32 nt_base = __COUNTER__ + 1;
	RuleSet rulesets[9];
#define RULES2(first, second) {first, second, -1u, -1u}
#define RULES4(f0, s0, f1, s1) RULES2(f0, s0), RULES2(f1, s1)
#define RULES6(f0, s0, f1, s1, f2, s2) RULES4(f0, s0, f1, s1), RULES2(f2, s2)
#define RULESET(cname, name, ...) \
	u16 rs_idx_##cname = (u16)(__COUNTER__ - nt_base); \
	u16 nt_##cname = NONTERM_BIT | rs_idx_##cname; \
	assert(rs_idx_##cname < array_count(rulesets)); \
	rulesets[rs_idx_##cname] = (RuleSet){ name, 0, VA_NARG(__VA_ARGS__) / 2, \
		{ CAT(RULES, VA_NARG(__VA_ARGS__))(__VA_ARGS__) } }
	RULESET(regkw_keyword, "keywords.register:keyword", TOK_STRING, TOK_NEWLINE);
	RULESET(regkw, "keywords.register", TOK_EXTRA_BASE + 0, nt_regkw_keyword);
	RULESET(regrule_body, "rules.register.native.hex:body",
	        TOK_RBRACE, TOK_NEWLINE,
	        TOK_NEWLINE, nt_regrule_body,
	        TOK_NUMBER, nt_regrule_body);
	RULESET(regrule_body_start, "rules.register.native.hex:body_start", TOK_LBRACE, nt_regrule_body);
	// TODO good position for adding an extension to test extensibility
	RULESET(regrule_name, "rules.register.native.hex:name", TOK_STRING, nt_regrule_body_start);
	RULESET(regrule_arch, "rules.register.native.hex:arch", TOK_IDENT, nt_regrule_name);
	RULESET(regrule, "rules.register.native.hex", TOK_EXTRA_BASE + 1, nt_regrule_arch);
	RULESET(toplevel, "toplevel",
	        nt_regkw, TOK_NONE,
		nt_regrule, TOK_NONE);
	RULESET(start, "start",
		nt_toplevel, nt_start,
		TOK_EOF, TOK_NONE);
#undef RULESET
#undef RULES6
#undef RULES4
#undef RULES2
	Parser parser = {
		&file,
		errors,
		array_count(extra_tokens),
		memdup(extra_tokens, sizeof(extra_tokens)),
		memdup(extra_token_strings, sizeof(extra_token_strings)),
		array_count(rulesets),
		memdup(rulesets, sizeof(rulesets)),
		0,
		NULL,
		0,
		NULL,
		malloc(array_count(rulesets) * sizeof(u32)),
		NULL,
	};
	u32 token_map[256] = {
		['"'] = TOK_STRING,
		['\n'] = TOK_NEWLINE,
		['0'] = TOK_NUMBER,
		['1'] = TOK_NUMBER,
		['2'] = TOK_NUMBER,
		['3'] = TOK_NUMBER,
		['4'] = TOK_NUMBER,
		['5'] = TOK_NUMBER,
		['6'] = TOK_NUMBER,
		['7'] = TOK_NUMBER,
		['8'] = TOK_NUMBER,
		['9'] = TOK_NUMBER,
		['('] = TOK_LPAREN,
		[')'] = TOK_RPAREN,
		['['] = TOK_LBRACKET,
		[']'] = TOK_RBRACKET,
		['{'] = TOK_LBRACE,
		['}'] = TOK_RBRACE,
	};
	print_parse_rules(&parser);
	regen_parse_table(&parser);
	Stack stack = {};
	stack.cap = 8;
	stack.data = malloc(stack.cap * sizeof(u16));
	stack_push(&stack, nt_start);
	b32 recovering = false;
	b32 found_error = false;
	u32 token_index = 0;
	while (stack.len) {
		// TODO should we precompute the token type?
		u32 token_offset, token_end;
		u32 token_len;
		u8 token_first;
		u8 token_second;
		do {
			token_offset = file.token_offsets[token_index++];
			if (token_offset == file.len) {
				token_end = token_offset;
				token_len = 0;
				token_first = token_second = 0;
			} else {
				token_end = file.token_offsets[token_index] - 1;
				token_len = token_end - token_offset + 1;
				token_first = file.data[token_offset];
				token_second = token_len >= 2 ? file.data[token_offset + 1] : 0;
			}
		} while (token_first == ' ' ||
		         token_first == '/' && (token_second == '/' || token_second == '*'));
		u32 token;
		if (token_len == 0) {
			token = TOK_EOF;
		} else {
			token = token_map[token_first];
			if (token == 0) {
				token = TOK_IDENT;
				// TODO make this more efficient
				for (u32 i = 0; i < parser.extra_token_count; i++) {
					// TODO limit size of extra tokens
					u32 len = (u32)strlen(parser.extra_tokens[i]);
					if (len == token_len &&
					    memcmp(file.data + token_offset,
					           parser.extra_tokens[i],
					           len) == 0) {
						token = TOK_EXTRA_BASE + i;
						break;
					}
				}
			}
		}
		while (stack.len) {
			u32 symbol = stack.data[--stack.len];
			if ((symbol & NONTERM_BIT) == 0) {
				if (token == symbol) {
					recovering = false;
					break;
				} else {
					if (!recovering) {
						report_parse_error(&parser, token_offset, token_end,
						                   ERROR "mismatched token, expected %0, got %1",
						                   token_to_string(symbol, parser.extra_tokens),
						                   token_to_string(token, parser.extra_tokens));
						found_error = true;
						recovering = true;
					}
					assert(symbol != TOK_EOF);
					if (token == TOK_EOF) {
						break;
					}
					if (TOK_BRACKETS_START <= symbol && symbol <= TOK_BRACKETS_END &&
					    TOK_BRACKETS_START <= token && token <= TOK_BRACKETS_END) {
						// TODO also do this if edit distance is sufficiently small
						break;
					}
					// TODO make sure we don't back out all the way and end the parse
				}
			} else {
				symbol &= ~NONTERM_BIT;
				u16 *entry = parser.parse_table + parser.parse_table_index[symbol];
				RuleSet *rs = &parser.rulesets[symbol];
				if (rs->count == 1) {
					Rule rule = rs->rules[0];
					if (rule.second != TOK_NONE) {
						stack_push(&stack, rule.second);
					}
					stack_push(&stack, rule.first);
					continue;
				}
				b32 found = false;
				for (u32 i = 0; i < rs->count && !found; i++) {
					while (true) {
						u32 word = *entry++;
						b32 is_last = word & END_BIT;
						u32 term = word &~ END_BIT;
						if (term == token) {
							Rule rule;
							if (i < array_count(rs->rules)) {
								rule = rs->rules[i];
							} else {
								i -= array_count(rs->rules);
								ExtraRules *rules = &parser.extra_rules[rs->next];
								while (i >= array_count(rules->rules)) {
									i -= array_count(rules->rules);
									rules = &parser.extra_rules[rules->next];
								}
								rule = rules->rules[i];
							}
							if (rule.second != TOK_NONE) {
								stack_push(&stack, rule.second);
							}
							stack_push(&stack, rule.first);
							found = true;
							break;
						}
						if (is_last) {
							break;
						}
					}
				}
				if (!found) {
					// TODO dump options
					if (!recovering) {
						report_parse_error(&parser, token_offset, token_end,
						                   ERROR "no match for token %0 in ruleset \"%1\"",
						                   token_to_string(token, parser.extra_tokens),
						                   rs->nonterminal);
						found_error = true;
						recovering = true;
					}
					// TODO handle no match
				}
			}
		}
	}
	// TODO handle ran out of stack -- we need to check if this was a failure

}

// Many arrays have a 32bit count. We ensure that these do not overflow by
// restricting the maximum file size to be safe in the worst case.
// If the file size is N, then the worst case values are:
// read buffer: N + 1 (we have to get a short read to ensure we have found the end)
// lines: N + 1
// tokens: N + 1
// brackets: N (our current balancing algorithm only deletes brackets)
// errors: N * num_passes (we cap this, so it does not contribute)
#define MAX_FILE_SIZE -2u
#define MAX_FILE_SIZE_STR "4GB - 2"

internal
int process_common_command_line(int argc, char *argv[static argc], SourceFile *vfile, ErrorCount *errors) {
	SourceFile file = {};
	*errors = (ErrorCount){};
	errors->use_color = (b32)isatty(STDERR_FILENO);
	errors->limit = 20;

	const int CODE_ERROR_LIMIT = 256;
	const int CODE_FORCE_COLOR = 257;
	struct option longopts[] = {
		{ "error-limit", required_argument, NULL, CODE_ERROR_LIMIT },
		{ "force-color", required_argument, NULL, CODE_FORCE_COLOR },
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
		case CODE_FORCE_COLOR:
		{
			if (*optarg == 0) {
				fprintf(stderr, "Empty argument to --force-color\n");
				return EX_USAGE;
			}
			if (strcmp(optarg, "on") == 0) {
				errors->use_color = true;
			} else if (strcmp(optarg, "off") == 0) {
				errors->use_color = false;
			} else {
				fprintf(stderr, "Argument to --force-color must be \"on\" or \"off\"\n");
				return EX_USAGE;
			}
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
		file.name = "<stdin>";
		file_stream = stdin;
	} else {
		file.name = argv[optind];
		file_stream = fopen(file.name, "r");
		if (file_stream == NULL) {
			perror(file.name);
			return EX_NOINPUT;
		}
	}
	s64 estimate = -1;
	if (fseek(file_stream, 0, SEEK_END) == 0) {
		estimate = ftell(file_stream);
		if (estimate != -1) {
			if (estimate > MAX_FILE_SIZE) {
				goto error_file_size;
			}
		}
		rewind(file_stream);
	}
	// We have to read one past the end to ensure that the file hasn't
	// changed size during the read
	u32 cap;
	if (estimate != -1) {
		cap = estimate > 8 ? (u32)estimate + 1 : 8;
	} else {
		cap = 4096;
	}
	file.data = malloc(cap);
	u32 histo[256] = {};
	while (true) {
		u32 wanted = cap - file.len;
		u32 result = (u32)fread(file.data + file.len, 1, wanted, file_stream);
		b32 done = false;
		if (result != wanted) {
			if (ferror(file_stream)) {
				perror(file.name);
				return EX_NOINPUT;
			}
			done = true;
		}
		for (u32 i = file.len; i < file.len + result; i++) {
			histo[file.data[i]]++;
		}
		file.len += result;
		if (file.len > MAX_FILE_SIZE) {
			goto error_file_size;
		}
		if (done) {
			break;
		}
		u64 newcap = (u64)cap + (cap >> 1);
		if (newcap > MAX_FILE_SIZE + 1) {
			newcap = MAX_FILE_SIZE + 1;
		}
		if (cap == MAX_FILE_SIZE + 1) {
			goto error_file_size;
		}
		cap = (u32)newcap;
		file.data = realloc(file.data, cap);
	}
	fclose(file_stream);
	if (file.len == 0) {
		*vfile = file;
		return 0;
	}
	if (estimate != -1 && file.len != estimate) {
		fprintf(stderr, "%s changed whilst reading", file.name);
		return EX_IOERR;
	}
	if (file.data[file.len - 1] != '\n') {
		fprintf(stderr, "%s is missing the trailing newline on the last line, adding...", file.name);
		if (file.len == MAX_FILE_SIZE) {
			goto error_file_size;
		}
		file.data = realloc(file.data, file.len + 1);
		file.data[file.len++] = '\n';
		histo['\n']++;
	} else {
		file.data = realloc(file.data, file.len);
	}
	file.line_count = histo['\n'] + 1; // Add a line for the EOF
	file.lines = malloc(file.line_count * sizeof(Line));
	// Overestimate -- comments may contain brackets too
	file.bracket_count = histo['('] + histo[')'] + histo['['] + histo[']'] + histo['{'] + histo['}'];
	file.brackets = malloc(file.bracket_count * sizeof(Bracket));
	*vfile = validate_utf8(file, errors);
	if (errors->fatal) {
		return 1;
	}
	return 0;

error_file_size:
	fprintf(stderr, "%s exceeds maximum file size of " MAX_FILE_SIZE_STR "\n", file.name);
	return EX_DATAERR;
}

internal
b32 print_error_summary(ErrorCount errors) {
	if (errors.count == 1) {
		fprintf(stderr, "1 error\n");
		return true;
	} else if (errors.count > 0) {
		if (!errors.limit || errors.count <= errors.limit) {
			fprintf(stderr, "%u errors\n", errors.count);
		} else {
			fprintf(stderr, "%u errors, only first %u reported\n", errors.count, errors.limit);
		}
		return true;
	}
	return false;
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

typedef struct {
	u32 indent;
	u32 align;
} Indent;

typedef struct {
	u8 *file_data;
	Indent *data;
	u32 len;
	u32 cap;
	u32 post_indent_column;
} FormatState;

internal
void print_newline(FormatState *state, Buf *buf, u32 token_start, u32 token_len, Indent *indent) {
	u32 given_indent = 0;
	u32 given_align = 0;
	u8 *token = state->file_data + token_start;
	{
		u32 i = 1;
		while (i < token_len && token[i] == '\t') {
			given_indent++;
			i++;
		}
		while (i < token_len && token[i] == ' ') {
			given_align++;
			i++;
		}
	}
	if (given_indent == indent->indent && given_align == indent->align) {
		buf_append(buf, state->file_data + token_start, token_len);
	} else {
		buf_push(buf, '\n');
		for (u32 i = 0; i < indent->indent; i++) {
			buf_push(buf, '\t');
		}
		for (u32 i = 0; i < indent->align; i++) {
			buf_push(buf, ' ');
		}
	}
	state->post_indent_column = 1 + indent->align;
}

internal
int format_main(int argc, char *argv[static argc]) {
	SourceFile file;
	ErrorCount errors;
	int result = process_common_command_line(argc, argv, &file, &errors);
	if (result != 0) {
		return result;
	}
	if (file.len == 0) {
		return 0;
	}
	file = tokenise(file, &errors);
	if (errors.fatal) {
		return 1;
	}
	balance_brackets(file, &errors);
	if (errors.fatal) {
		return 1;
	}
	parse(file, &errors);
	if (errors.fatal) {
		return 1;
	}

	FormatState fmt_state;
	fmt_state.cap = 8;
	fmt_state.data = malloc(fmt_state.cap * sizeof(Indent));
	fmt_state.len = 1;
	fmt_state.data[0] = (Indent){};
	fmt_state.post_indent_column = 1;
	fmt_state.file_data = file.data;
	u32 prev_token_offset;
	u8 prev_token_first;
	u32 prev_token_len;
	u32 token_offset = -1u;
	u8 token_first = 0;
	u32 token_len = 0;
	u32 index = 0;
	u32 bracket_index = 0;
	{
		// Skip indent on first line
		u8 c = file.data[file.token_offsets[index]];
		if (c == '\t' || c == ' ') {
			index++;
		}
	}
	Buf output = {};
	output.cap = file.len;
	output.data = malloc(output.cap);
	while (token_offset != file.len) {
		prev_token_offset = token_offset;
		prev_token_first = token_first;
		prev_token_len = token_len;
		token_offset = file.token_offsets[index++];
		if (token_offset != file.len) {
			token_first = file.data[token_offset];
			token_len = file.token_offsets[index] - token_offset;
		} else {
			token_first = 0;
			token_len = 0;
		}
		switch (token_first) {
		case '(':
		case ')':
		case '[':
		case ']':
		case '{':
		case '}':
		{
			if (file.brackets[bracket_index++].c == 0) {
				goto ignore_bracket;
			}
			Indent *indent;
			assert(token_len == 1);
			u8 type = BRACKET_TYPE(token_first);
			if (token_first == open_bracket[type]) {
				if (fmt_state.len == fmt_state.cap) {
					fmt_state.cap += fmt_state.cap >> 1;
					fmt_state.data = realloc(fmt_state.data, fmt_state.cap * sizeof(Indent));
				}
				indent = &fmt_state.data[fmt_state.len - 1];
				Indent *new = &fmt_state.data[fmt_state.len++];
				if (token_first == '{') {
					new->indent = indent->indent + 1;
					new->align = 0;
				} else {
					new->indent = indent->indent;
					new->align = fmt_state.post_indent_column;
				}
			} else {
				fmt_state.len--;
				indent = &fmt_state.data[fmt_state.len];
				if (type == 2) {
					indent->indent--;
				}
			}
			if (prev_token_first == '\n') {
				print_newline(&fmt_state, &output, prev_token_offset, prev_token_len, indent);
			}
			fmt_state.post_indent_column++;
			buf_append(&output, file.data + token_offset, 1);
			break;
		}
		case '\n':
		case 0:
		{
			if (prev_token_first == '\n') {
				print_newline(&fmt_state, &output, prev_token_offset, prev_token_len,
				              &fmt_state.data[0]);
			}
			break;
		}
		default:
		{
ignore_bracket:
			if (prev_token_first == '\n') {
				Indent *indent = &fmt_state.data[fmt_state.len - 1];
				print_newline(&fmt_state, &output, prev_token_offset, prev_token_len, indent);
			}
			// TODO column vs character vs byte count
			fmt_state.post_indent_column += token_len;
			buf_append(&output, file.data + token_offset, token_len);
			break;
		}
		}
	}
	// TODO don't free memory that is about to be freed by program exit
	free(fmt_state.data);
	if (print_error_summary(errors)) {
		fwrite(output.data, 1, output.len, stdout);
		return 1;
	}
	fwrite(output.data, 1, output.len, stdout);
	return 0;
}

internal
void usage() {
	fprintf(stderr,
	        "usage: mepa <command> [<options>] [<args>]\n"
	        "\n"
	        "Commands:\n"
	        "    help              - show this usage text\n"
	        "    format  [<file>]  - reformat code\n"
	        "    compile [<file>]  - compile code\n");
}

internal
int help_main(int argc, char *argv[static argc]) {
	(void) argc;
	(void) argv;
	usage();
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
