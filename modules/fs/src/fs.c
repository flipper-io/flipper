#define __private_include__
#include <flipper/fs.h>
#include <private/nvm.h>

/* Declare all of the global variables for this module. */
nvm_p _free_list;
nvm_p _break_value;
nvm_p _root_leaf;
nvm_p _rw_head;

/* Define the virtual interface for this module. */
const struct _fs fs = {
	fs_configure,
	fs_create,
	fs_delete,
	fs_size,
	fs_open,
	fs_get,
	fs_push,
	fs_pull,
	fs_close,
	fs_format,
	#ifdef __fs_transfer_symbols__
	fs_transfer,
	fs_receive
	#endif
};
