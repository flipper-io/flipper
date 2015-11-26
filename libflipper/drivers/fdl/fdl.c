#define __private_include__

#include <fdl/fdl.h>

#include <fmr/fmr.h>

#include <platform/fmr.h>

void fdl_configure(void) {
    
    
    
}

void fdl_activate(uint16_t key) {
    
    host.invoke(_fdl, _fdl_activate, 2, little(key), 0);
    
}