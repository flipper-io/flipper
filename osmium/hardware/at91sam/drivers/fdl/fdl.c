#define __private_include__

#include <fdl/fdl.h>

#include <fs/fs.h>

#include <flash/flash.h>

#include <fs/tree.h>

#include <usart/usart.h>

#include <io/io.h>

void fdl_configure(void) {
    
	io.direction(7, OUTPUT);
	
	io.write(7, ON);
	
}

void fdl_load(uint16_t key) {
    
    fsp _leaf = fs_leaf_for_key(_root_leaf, key);
    
    if (!_leaf) {
        
        /* ~ There is no leaf to match the key provided. ~ */
        
        return;
        
    }
    
    /* ~ Dereference the metadata contained by the leaf. ~ */
    
    leaf *l = flash_dereference(_leaf, sizeof(leaf));
    
    /* ~ Move the loadable from external memory into internal memory. ~ */
    
    void *init = flash_dereference(l -> data, l -> size);
	
	/* ~ Release the memory allocated to dereference the leaf. ~ */
	
	free(l);
	
	if (!init) return;
	
    /* ~ Configure the ABI. ~ */
	
	*(struct _fdl **)(init + 0x4) = (struct _fdl *)(&fdl);
	
	(*(struct _fdl **)(init + 0x4)) -> configure();
	
	//memcpy((void *)(init + 0x4), &fdl, sizeof(const struct _fdl));
	
    /* ~ Call the initialization routine. ~ */
    
    ((void (*)(void))(init))();
	
    free(init);
    
}

void fdl_resolve(uint16_t key, const void *address) {
	
	
	
}