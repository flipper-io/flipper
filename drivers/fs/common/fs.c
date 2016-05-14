#define __private_include__
#include <flipper/fs.h>

/* Declare all of the global variables for this module. */
fsp _free_list;
fsp _break_value;
fsp _root_leaf;

/* Define the virtual interface for this module. */
const struct _fs fs = {
	fs_configure,
	fs_format,
	fs_data,
#ifndef __osmium__
	fs_upload,
	fs_download
#endif
};
