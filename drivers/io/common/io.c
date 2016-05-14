#define __private_include__
#include <flipper/io.h>

/* Define the virtual interface for this module. */
const struct _io io = {
	io_configure,
	io_set_direction,
	io_write,
	io_read
};
