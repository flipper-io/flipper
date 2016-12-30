#define __private_include__
#include <flipper/carbon/modules/fs.h>
#include <nvm.h>
#include <flipper/error.h>

/* This function traverses the filesystem tree starting at a leaf pointed to by the 'current' argument until an empty branch pointer corresponding to the given key is found. */
nvm_p fs_empty_branch_for_key(nvm_p _branch, nvm_p current, uint16_t key) {
	/* If our 'current' pointer is zero, then an empty branch pointer has been found; return it. */
	if (current == 0) {
		return _branch;
	}
	/* Dereference the 'current' pointer to bring a copy of the leaf that it points to into local memory. */
	leaf *_current = nvm_dereference(current, sizeof(leaf));
	/* Create a locally scoped variable to hold the result of the search. */
	nvm_p _empty = 0;
	/* If the key of the current leaf is less than the key we are searching for, recursively search its left child. */
	if (_current -> key < key) {
		_empty = fs_empty_branch_for_key(fs_access(current, leaf, left), _current -> left, key);
	}
	/* If the key of the current leaf is greater than the key we are searching for, recursively search its right child. */
	else if (_current -> key > key) {
		_empty = fs_empty_branch_for_key(fs_access(current, leaf, right), _current -> right, key);
	}
	/* Free the memory allocated to dereference the current leaf. */
	free(_current);
	/* Return the empty leaf pointer (if any). */
	return _empty;
}

/* This function indexes a leaf into the filesystem tree by allocating memory for it and setting its key. */
nvm_p fs_add_leaf_with_key(nvm_p current, uint16_t key) {
	/* Walk the filesystem tree until an empty branch pointer is found to match the key given. This will be where we will index the newly allocated leaf. */
	nvm_p _branch = fs_empty_branch_for_key(0, current, key);
	/* Check to ensure that the leaf we aim to create doesn't already exist. */
	if (_branch) {
		/* Allocate memory for the leaf. */
		nvm_p region = nvm_alloc(sizeof(leaf));
		if (!region) {
			error_raise(E_MALLOC, error_message("Failed to allocate the external memory required to create a new filesystem leaf."));
			return 0;
		}
		/* Create the new leaf. */
		leaf *_leaf = malloc(sizeof(leaf));
		if (!_leaf) {
			error_raise(E_MALLOC, error_message("Failed to allocate the memory required to create a new filesystem leaf."));
			return 0;
		}
		/* Clear the new leaf. */
		memset(_leaf, 0, sizeof(leaf));
		/* Index the leaf in the tree by overwriting by writing the newly allocated memory region to the empty branch pointer we found earlier. */
		nvm_push(&region, sizeof(nvm_p), _branch);
		/* Associate the given key with the new leaf by writing the key given into the key property of the new leaf. */
		_leaf -> key = key;
		/* Write the empty branch pointer we found earlier into the branch pointer property of the new leaf. */
		_leaf -> _branch = _branch;
		/* Send the new leaf to the filesystem. */
		nvm_push(_leaf, sizeof(leaf), region);
		/* Free memory allocated for the leaf. */
		free(_leaf);
		/* Return a pointer to the newly created leaf. */
		return region;
	}
	/* This case catches an exception wherein the leaf we want to add already exists. */
	else {
		error_raise(E_FS_EXISTS, error_message("Could not create a file with the key '0x%04x'.", key));
	}
	return 0;
}

/* This function traverses the filesystem tree until a leaf matching a given key is found. */
nvm_p fs_leaf_for_key(nvm_p current, uint16_t key) {
	/* If we reach the end of the tree and have not yet found a matching key, then the leaf we are interested in finding does not exist. */
	if (current == 0) {
		error_raise(E_FS_NO_FILE, error_message("There is no file with the key '0x%04x'.", key));
		return 0;
	}
	/* However, if we have not yet reached the end of the tree, dereference the 'current' leaf pointer to bring a copy of it into local memory. */
	leaf *_current = nvm_dereference(current, sizeof(leaf));
	/* Create a locally scoped variable to hold the result of the search. */
	nvm_p _leaf = 0;
	/* Return a pointer to the current leaf if its key matched the one we are searching for. */
	if (_current -> key == key) {
		_leaf = current;
	}
	/* If the key of the current leaf is less than the key we are searching for, recursively search its left child. */
	else if (_current -> key < key) {
		_leaf = fs_leaf_for_key(_current -> left, key);
	}
	/* If the key of the current leaf is greater than the key we are searching for, recursively search its right child. */
	else if (_current -> key > key) {
		_leaf = fs_leaf_for_key(_current -> right, key);
	}
	/* Free the memory allocated to dereference the current leaf. */
	free(_current);
	/* Return the leaf (if any). */
	return _leaf;
}

int fs_remove_leaf_with_key(nvm_p parent, uint16_t key) {
	/* Iterate through the filesystem tree until we find the leaf that we'd like to delete. */
	suppress_errors(nvm_p match = fs_leaf_for_key(parent, key));
	/* Catch the edge case in which the leaf that we're trying to delete doesn't exist. */
	if (!match) {
		error_raise(E_FS_NO_FILE, error_message("Could not remove the leaf with the key '0x%04x'.", key));
		return -1;
	}
	/* Otherwise, dereference it to bring a copy of it into local memory. */
	leaf *_match = nvm_dereference(match, sizeof(leaf));
	/* Check the worst case scenario first; the leaf we want to delete has two children. */
	if (_match -> left && _match -> right) {
		/* De-index the leaf by replacing its branch pointer with a pointer to its left child. */
		nvm_push(&(_match -> left), sizeof(nvm_p), _match -> _branch);
		/* Change the branch pointer of the leaf's left child to match its new index in the filesystem tree. */
		nvm_push(&(_match -> _branch), sizeof(nvm_p), fs_access(_match -> left, leaf, _branch));
		/* Walk the left child of the leaf to find an empty branch pointer to which the leaf's orphaned right child can be appended. */
		uint16_t *key = (uint16_t *) nvm_dereference(fs_access(_match -> right, leaf, key), sizeof(uint16_t));
		nvm_p *right = (nvm_p *) nvm_dereference(fs_access(_match -> left, leaf, right), sizeof(nvm_p));
		nvm_p empty = fs_empty_branch_for_key(fs_access(_match -> left, leaf, right), *right, *key);
		if (!empty) {
			error_raise(E_FS_EXISTS, error_message("Could not move a child leaf while trying to delete the file with key '0x%04x'.", *key));
		}
		/* Re-index the orphaned right child by writing its address into the empty branch pointer we found. */
		nvm_push(&(_match -> right), sizeof(nvm_p), empty);
		/* Overwrite the branch pointer of the leaf's right child to reflect its new index in the filesystem tree. */
		nvm_push(&empty, sizeof(nvm_p), fs_access(_match -> right, leaf, _branch));
	}
	/* Not a bad scenario; the leaf we wish to delete only has a left child. */
	else if (_match -> left) {
		/* De-index the leaf by replacing its branch pointer with a pointer to its left child. */
		nvm_push(&(_match -> left), sizeof(nvm_p), _match -> _branch);
		/* Change the branch pointer of the leaf's left child to match its new index in the filesystem tree. */
		nvm_push(&(_match -> _branch), sizeof(nvm_p), fs_access(_match -> left, leaf, _branch));
	}
	/* Also not a bad scenario; the leaf we wish to delete only has a right child. */
	else if (_match -> right) {
		/* De-index the leaf by replacing its branch pointer with a pointer to its right child. */
		nvm_push(&(_match -> right), sizeof(nvm_p), _match -> _branch);
		/* Change the branch pointer of the leaf's right child to match its new index in the filesystem tree. */
		nvm_push(&(_match -> _branch), sizeof(nvm_p), fs_access(_match -> right, leaf, _branch));
	}
	/* Best case scenario, the leaf to be deleted has no children. */
	else {
		/* De-index the leaf from the tree by clearing its branch pointer. */
		nvm_p zero = 0;
		nvm_push(&zero, sizeof(nvm_p), _match -> _branch);
	}
	/* Free any memory allocated to index the leaf. */
	nvm_free(match);
	/* Free the memory allocated to store the local copy of the match. */
	free(_match);
	/* Return with success. */
	return 0;
}
