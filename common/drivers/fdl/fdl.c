#define __private_include__

#include <fdl/fdl.h>

const struct _fdl fdl = {
    
#ifndef __atmega_build__
	
	fdl_configure,
	
	fdl_load,
	
	fdl_resolve
	
#endif
	
};