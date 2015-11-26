#define __private_include__

#include <fdl/fdl.h>

#include <fs/fs.h>

#include <flash/flash.h>

#include <fs/tree.h>

#include <usart/usart.h>

void fdl_configure(void) {
    
    
    
}

void fdl_activate(uint16_t key) {
    
    fsp _leaf = fs_leaf_for_key(_root_leaf, key);
    
    if (!_leaf) {
        
        /* ~ There is no leaf to match the key provided. ~ */
        
        return;
        
    }
    
    /* ~ Dereference the metadata contained by the leaf. ~ */
    
    leaf *l = flash_dereference(_leaf, sizeof(leaf));
    
    /* ~ Move the loadable from external memory into internal memory. ~ */
    
    void *init = flash_dereference(l -> data, l -> size);
    
    /* ~ Configure the ABI. ~ */
    
    /* ~ Call the initialization routine. ~ */
    
    ((void (*)(void))(init))();
    
    free(l);
    
    free(init);
    
}