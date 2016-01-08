1i\
typedef struct { \
	u32 start; \
	u32 end; \
} UnicodeRange; \
\
internal \
UnicodeRange xid_start_table[] = {
$a\
};
s/ *[#;].*//
/^$/d
s/^\([0-9A-F]\+\)$/\t{0x\1, 0x\1},/
s/^\([0-9A-F]\+\)..\([0-9A-F]\+\)$/\t{0x\1, 0x\2},/
