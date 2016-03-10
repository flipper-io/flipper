#define __private_include__
#include <fs/fs.h>
#include <platform.h>

void fs_configure(void) {
	
	/* ~ We have to load the freelist and the _break_value in from memory! ~ */
	at45_pull(&_free_list, sizeof(fsp), _FREE_LIST);
	at45_pull(&_break_value, sizeof(fsp), _BREAK_VALUE);
	at45_pull(&_root_leaf, sizeof(fsp), _ROOT_LEAF);
	
}

void fs_format(void) {
	
}