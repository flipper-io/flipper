#define __private_include__

#include <fdl/fdl.h>

#include <fmr/fmr.h>

void fdl_configure(void) {
    
    
    
}

void *fdl_load(uint16_t key) {
    
    return (void *)(uint64_t)(host.invoke(_fdl, _fdl_load, 2, little(key), 0));
    
}

void fdl_resolve(uint16_t key, const void *address) {
	
	
	
}