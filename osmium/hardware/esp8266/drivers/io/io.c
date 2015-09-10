#define __private_include__

#include <drivers/io.h>

const struct _io io = {
	
	io_configure,
	
	io_set_direction,
	
	io_write,
	
	io_read
	
};