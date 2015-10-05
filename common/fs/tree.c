#define __private_include__

#include <fs/tree.h>

#include <flash/flash.h>

/* ~ This function traverses the filesystem tree starting at a leaf pointed to by the 'current' argument until an empty branch pointer corresponding to the given key is found. ~ */

fsp fs_empty_branch_for_key(fsp _branch, fsp current, uint16_t key) {
	
	/* ~ If our 'current' pointer is zero, then an empty branch pointer has been found; return it. ~ */
	
	if (!current) return _branch;
	
	/* ~ Dereference the 'current' pointer to bring a copy of the leaf that it points to into local memory. ~ */
	
	leaf *_current = flash_dereference(current, sizeof(leaf));
	
	/* ~ Since we're here, we haven't yet found an empty branch pointer; walk the filesystem tree recursively until we do. ~ */
	
	if (_current -> key < key)
	
		return fs_empty_branch_for_key(forward(current, leaf, left), _current -> left, key);
	
	else if (_current -> key > key)
	
		return fs_empty_branch_for_key(forward(current, leaf, right), _current -> right, key);
	
	else {
		
		/* ~ This case catches an exception wherein the key we want to be empty is not. i.e. A leaf with the given key already exists. ~ */
		
		return -1;
		
	}
	
}

/* ~ This function indexes a leaf into the filesystem tree by allocating memory for it and setting its key. ~ */

fsp fs_add_leaf_with_key(fsp current, uint16_t key) {
	
	/* ~ Walk the filesystem tree until an empty branch pointer is found to match the key given. This will be where we will index the newly allocated leaf. ~ */
	
	fsp _branch = fs_empty_branch_for_key(0, current, key);
	
	/* ~ Check to ensure that the leaf we aim to create doesn't already exist. ~ */
	
	if (_branch != -1) {
		
		/* ~ Allocate memory for the leaf. ~ */
		
		fsp region = (fsp) flash_alloc(sizeof(leaf));
		
		/* ~ Index the leaf in the tree by overwriting by writing the newly allocated memory region to the empty branch pointer we found earlier. ~ */
		
		flash_push(&region, sizeof(fsp), _branch);
		
		/* ~ Associate the given key with the new leaf by writing the key given into the key property of the new leaf. ~ */
		
		flash_push(&key, sizeof(uint16_t), forward(region, leaf, key));
		
		/* ~ Write the empty branch pointer we found earlier into the branch pointer property of the new leaf. ~ */
		
		flash_push(&_branch, sizeof(fsp), forward(region, leaf, _branch));
		
		/* ~ Return a pointer to the newly created leaf. ~ */
		
		return region;
		
	}
	
	else {
		
		/* ~ This case catches an exception wherein the leaf we want to add already exists. ~ */
		
		verbose("An attempt was made to add a leaf that already exists.");
		
		return 0;
		
	}
	
	return 0;
	
}

/* ~ This function traverses the filesystem tree until a leaf matching a given key is found. ~ */

fsp fs_leaf_for_key(fsp current, uint16_t key) {
	
	/* ~ If we reach the end of the tree and have not yet found a matching key, then the leaf we are interested in finding does not exist. ~ */
	
	if (!current) {
		
		verbose("No leaf was found to match the key given, 0x%04X.\n\n", key);
		
		return 0;
		
	}
	
	/* ~ However, if we have not yet reached the end of the tree, dereference the 'current' leaf pointer to bring a copy of it into local memory. ~ */
	
	leaf *_current = flash_dereference(current, sizeof(leaf));
	
	if (_current -> key == key)
	
		return current;
	
	else if (_current -> key < key)
	
		return fs_leaf_for_key(_current -> left, key);
	
	else if (_current -> key > key)
	
		return fs_leaf_for_key(_current -> right, key);
	
	return 0;
	
}

void fs_remove_leaf_with_key(fsp parent, uint16_t key) {
	
	/* ~ Iterate through the filesystem tree until we find the leaf that we'd like to delete. ~ */
	
	fsp match = fs_leaf_for_key(parent, key);
	
	/* ~ If the leaf that we're trying to delete doesn't exist, laugh it off. ~ */
	
	if (!match) {
		
		verbose("An attempt was made to remove a non-existent leaf.\n\n");
		
		return;
		
	}
	
	/* ~ Otherwise, dereference it to bring a copy of it into local memory. ~ */
	
	leaf *_match = flash_dereference(match, sizeof(leaf));
	
	/* ~ Check the worst case scenario first; the leaf we want to delete has two children. Ugh. ~ */
	
	if (_match -> left && _match -> right) {
		
		verbose("Deleting a leaf with two children.\n\n");
		
		/* ~ De-index the leaf by replacing its branch pointer with a pointer to its left child. ~ */
		
		flash_push(&(_match -> left), sizeof(fsp), _match -> _branch);
		
		/* ~ Change the branch pointer of the leaf's left child to match its new index in the filesystem tree. ~ */
		
		flash_push(&(_match -> _branch), sizeof(fsp), forward(_match -> left, leaf, _branch));
		
		/* ~ Walk the left child of the leaf to find an empty branch pointer to which the leaf's orphaned right child can be appended. ~ */
		
		uint16_t *key = (uint16_t *) flash_dereference(forward(_match -> right, leaf, key), sizeof(uint16_t));
		
		fsp *right = (fsp *) flash_dereference(forward(_match -> left, leaf, right), sizeof(fsp));
		
		fsp empty = fs_empty_branch_for_key(forward(_match -> left, leaf, right), *right, *key);
		
		/* ~ Re-index the orphaned right child by writing its address into the empty branch pointer we found. ~ */
		
		flash_push(&(_match -> right), sizeof(fsp), empty);
		
		/* ~ Overwrite the branch pointer of the leaf's right child to reflect its new index in the filesystem tree. ~ */
		
		flash_push(&empty, sizeof(fsp), forward(_match -> right, leaf, _branch));
		
	}
	
	/* ~ Not a bad scenario; the leaf we wish to delete only has a left child. ~ */
	
	else if (_match -> left) {
		
		verbose("Deleting a leaf with a left child.\n\n");
		
		/* ~ De-index the leaf by replacing its branch pointer with a pointer to its left child. ~ */
		
		flash_push(&(_match -> left), sizeof(fsp), _match -> _branch);
		
		/* ~ Change the branch pointer of the leaf's left child to match its new index in the filesystem tree. ~ */
		
		flash_push(&(_match -> _branch), sizeof(fsp), forward(_match -> left, leaf, _branch));
		
	}
	
	/* ~ Also not a bad scenario; the leaf we wish to delete only has a right child. ~ */
	
	else if (_match -> right) {
  
		verbose("Deleting a leaf with a right child.\n\n");
		
		/* ~ De-index the leaf by replacing its branch pointer with a pointer to its right child. ~ */
		
		flash_push(&(_match -> right), sizeof(fsp), _match -> _branch);
		
		/* ~ Change the branch pointer of the leaf's right child to match its new index in the filesystem tree. ~ */
		
		flash_push(&(_match -> _branch), sizeof(fsp), forward(_match -> right, leaf, _branch));
		
	}
	
	else {
		
		/* ~ Best case scenario, the leaf to be deleted has no children. ~ */
		
		verbose("Deleting a leaf with no children.\n\n");
		
		/* ~ De-index the leaf from the tree by clearing its branch pointer. ~ */
		
		fsp zero = 0;
		
		flash_push(&zero, sizeof(fsp), _match -> _branch);
		
	}
	
	/* ~ Free any memory allocated to index the leaf. ~ */
	
	flash_free(match);
	
	/* ~ Free the memory allocated to store the local copy of the match. ~ */
	
	free(_match);
	
}