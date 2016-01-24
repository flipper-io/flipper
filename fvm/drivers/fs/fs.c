#define __private_include__

#include <fs/fs.h>

#include <fs/tree.h>

#include <at45/at45.h>

void fs_format(void) {
	
	/* ~ Reset the allocation table. ~ */
	
	_free_list = 0;
	
	at45_push(&_free_list, sizeof(fsp), _FREE_LIST);
	
	_break_value = 528;
	
	at45_push(&_break_value, sizeof(fsp), _BREAK_VALUE);
	
	/* ~ Create the root leaf. ~ */
	
	leaf *root = (leaf *) malloc(sizeof(leaf));
	
	root -> key = 0x4321; //checksum(root_name, root_namelen);
	
	root -> left = 0;
	
	root -> right = 0;
	
	/* ~ Allocate space for the root leaf. ~ */
	
	_root_leaf = at45_alloc(sizeof(leaf));
	
	/* ~ Reset the root leaf pointer. ~ */
	
	at45_push(&_root_leaf, sizeof(fsp), _ROOT_LEAF);
	
	/* ~ Write the root leaf into memory. ~ */
	
	at45_push(root, sizeof(leaf), _root_leaf);
	
	/* ~ Free the memory allocated to hold the leaf. ~ */
	
	free(root);
	
}