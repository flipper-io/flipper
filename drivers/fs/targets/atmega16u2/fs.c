#define __private_include__
#include <flipper/fs.h>
#include <flipper/error.h>
#include <flipper/nvm.h>
#include <flipper/fs/tree.h>
#include <flipper/platform/platform.h>

void fs_configure(void) {

	/* We have to load the freelist and the _break_value in from memory! */
	nvm_pull(&_free_list, sizeof(fsp), _FREE_LIST);
	nvm_pull(&_break_value, sizeof(fsp), _BREAK_VALUE);
	nvm_pull(&_root_leaf, sizeof(fsp), _ROOT_LEAF);

}

void fs_format(void) {

	/* Reset the allocation table. */
	_free_list = 0;
	nvm_push(&_free_list, sizeof(fsp), _FREE_LIST);
	_break_value = DEFAULT_BREAK_VALUE;
	nvm_push(&_break_value, sizeof(fsp), _BREAK_VALUE);

	/* Create the root leaf. */
	leaf *root = (leaf *) malloc(sizeof(leaf));

	if (!root) {
		error_raise(E_NO_MEM, "");
		// TODO: not sure what sort of cleanup is necessary here.
		return;
	}

	memset(root, 0, sizeof(leaf));
	root -> key = 0x4321;

	/* Allocate space for the root leaf. */
	_root_leaf = nvm_alloc(sizeof(leaf));

	/* Reset the root leaf pointer. */
	nvm_push(&_root_leaf, sizeof(fsp), _ROOT_LEAF);

	/* Write the root leaf into memory. */
	nvm_push(root, sizeof(leaf), _root_leaf);

	/* Free the memory allocated to hold the leaf. */
	free(root);

}

fsp fs_data(char *name) {

	return 0;

}