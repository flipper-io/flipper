#define __private_include__
#include <flipper/carbon/fs.h>

#ifdef __use_fs__
/* Define the virtual interface for this module. */
const struct _fs fs = {
	fs_configure,
	fs_create,
	fs_delete,
	fs_open,
	fs_size,
	fs_seek,
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
#endif
