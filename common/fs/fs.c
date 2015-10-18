#define __private_include__

#include <fs/fs.h>

#include <flash/flash.h>

#include <fs/tree.h>

fsp _root_leaf;

fsp _free_list;

fsp _break_value;

const struct _fs fs = {
	
	fs_configure,
	
	fs_format
	
};

void fs_configure(void) {
	
	/* ~ We have to load the freelist and the _break_value in from memory! ~ */
	
	flash.pull(&_free_list, sizeof(fsp), _FREE_LIST);
	
	flash.pull(&_break_value, sizeof(fsp), _BREAK_VALUE);
	
	flash.pull(&_root_leaf, sizeof(fsp), _ROOT_LEAF);
	
}