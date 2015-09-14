#define __private_include__

#include <flash/flash.h>

const struct _flash flash = {
	
	flash_configure,
	
	flash_enable,
	
	flash_disable,
	
	flash_reset,
	
	flash_alloc,
	
	flash_free,
	
	flash_format,
	
	flash_push,
	
	flash_pull,
	
	flash_dereference
	
};