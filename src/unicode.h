#include "common.h"

typedef struct {
	u16 start;
	u16 end;
} UnicodeRange16;


typedef struct {
	u32 start;
	u32 end;
} UnicodeRange32;

#include "xid_start_table.gen.h"
#include "xid_continue_table.gen.h"
