#define __private_include__

#include <fs/fs.h>

fsp _free_list;

fsp _break_value;

fsp _root_leaf;

const struct _fs fs = {
	
	fs_configure,
	
	fs_format
	
};