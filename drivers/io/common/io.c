#define __private_include__
#include <flipper/io/io.h>

/* ~ Define the virtual driver object. ~ */
const struct _io io = {

	io_configure,
	io_set_direction,
	io_write,
	io_read

};
