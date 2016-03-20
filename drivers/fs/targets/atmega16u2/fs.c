#define __private_include__
#include <fs/fs.h>
#include <platform.h>
#include <at45/at45.h>
#include <fs/tree.h>

void fs_configure(void) {

	/* ~ We have to load the freelist and the _break_value in from memory! ~ */
	at45_pull(&_free_list, sizeof(fsp), _FREE_LIST);
	at45_pull(&_break_value, sizeof(fsp), _BREAK_VALUE);
	at45_pull(&_root_leaf, sizeof(fsp), _ROOT_LEAF);

}

void fs_format(void) {

	/* ~ Reset the allocation table. ~ */
	_free_list = 0;
	at45_push(&_free_list, sizeof(fsp), _FREE_LIST);
	_break_value = 528;
	at45_push(&_break_value, sizeof(fsp), _BREAK_VALUE);

	/* ~ Create the root leaf. ~ */
	leaf *root = (leaf *) malloc(sizeof(leaf));

	if(!root) {
		error.raise(E_NO_MEM, "Out of memory.\n");
		// TODO: not sure what sort of cleanup is necessary here.
		return;

	memset(root, 0, sizeof(leaf));
	root -> key = 0x4321;

	/* ~ Allocate space for the root leaf. ~ */
	_root_leaf = at45_alloc(sizeof(leaf));

	/* ~ Reset the root leaf pointer. ~ */
	at45_push(&_root_leaf, sizeof(fsp), _ROOT_LEAF);

	/* ~ Write the root leaf into memory. ~ */
	at45_push(root, sizeof(leaf), _root_leaf);

	/* ~ Free the memory allocated to hold the leaf. ~ */
	free(root);

}
