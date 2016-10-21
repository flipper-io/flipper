#define __private_include__
#include <flipper/fs.h>
#include <private/nvm.h>

/* Declare all of the global variables for this module. */
nvm_p _free_list;
nvm_p _break_value;
nvm_p _root_leaf;

/* Define the virtual interface for this module. */
const struct _fs fs = {
	fs_configure,
	fs_format,
	fs_create,
	fs_remove,
	fs_rename,
	fs_write,
	nvm_put,
	fs_read,
	nvm_get,
	fs_data,
#ifdef __fs_transfer_symbols__
	fs_transfer,
	fs_receive
#endif
};
