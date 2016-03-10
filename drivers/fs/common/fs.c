#define __private_include__
#include <fs/fs.h>

/* ~ Declare all global variables. ~ */
fsp _free_list;
fsp _break_value;
fsp _root_leaf;

/* ~ Define the virtual driver object. ~ */
const struct _fs fs = {

	fs_configure,
	fs_format

};