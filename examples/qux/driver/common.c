#define __private_include__

#include "qux.h"

#ifdef __device_compilation__

__attribute__((section(".fdl")))

#endif

const struct _qux qux = {
	
	qux_configure,
	
	qux_on,
	
	qux_off
	
};