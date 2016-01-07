#include "common.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

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
	if (argc == 2 || strcmp(argv[2], "-") == 0) {
		file = stdin;
	} else {
		file = nonnull_or_die(fopen(argv[2], "r"));
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
	fwrite(contents, 1, size, stdout);
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
