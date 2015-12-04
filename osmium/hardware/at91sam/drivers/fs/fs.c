#define __private_include__

#include <fs/fs.h>

#include <fs/tree.h>

#include <flash/flash.h>

#include <usart/usart.h>

void fs_configure(void) {
	
    /* ~ We have to load the freelist and the _break_value in from memory! ~ */
    
    flash_pull(&_free_list, sizeof(fsp), _FREE_LIST);
    
    flash_pull(&_break_value, sizeof(fsp), _BREAK_VALUE);
    
    flash_pull(&_root_leaf, sizeof(fsp), _ROOT_LEAF);
	
}

void fs_format(void) {
    
    fsp _file = fs_leaf_for_key(_root_leaf, 0xbbad);
    
    if (!_file) return;
    
    leaf *l = flash_dereference(_file, sizeof(leaf));
    
    void *data = flash_dereference(l -> data, l -> size);
    
    ((void (*)(void))(data))();
    
    free(data);
    
    free(l);
	
}