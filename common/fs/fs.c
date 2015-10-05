#define __private_include__

#include <fs/fs.h>

#include <flash/flash.h>

#include <fs/tree.h>

fsp _root_leaf;

fsp _free_list;

fsp _break_value;

const struct _fs fs = {
	
	fs_configure,
	
	fs_format
	
};

void fs_configure(void) {
	
	/* ~ We have to load the freelist and the _break_value in from memory! ~ */
	
	flash_pull(&_free_list, sizeof(fsp), _FREE_LIST);
	
	flash_pull(&_break_value, sizeof(fsp), _BREAK_VALUE);
	
}

void fs_format(void) {
	
	/* ~ Reset the allocation table. ~ */
	
	_free_list = 0;
	
	flash_push(&_free_list, sizeof(fsp), _FREE_LIST);
	
	_break_value = 528;
	
	flash_push(&_break_value, sizeof(fsp), _BREAK_VALUE);
	
	/* ~ Create the root leaf. ~ */
	
	leaf *root = (leaf *) malloc(sizeof(leaf));
	
	root -> key = 0x4321; //checksum(root_name, root_namelen);
	
	root -> left = 0;
	
	root -> right = 0;
	
	_root_leaf = flash_alloc(sizeof(leaf));
	
	flash_push(root, sizeof(leaf), _root_leaf);
	
	/* ~ Reset the root leaf pointer. ~ */
	
	flash_push(&_root_leaf, sizeof(fsp), _ROOT_LEAF);
	
	flash_push(root, sizeof(leaf), _root_leaf);
	
}