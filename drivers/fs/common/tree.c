#define __private_include__
#include <flipper/error.h>
#include <flipper/fs/tree.h>
#include <flipper/at45.h>

/* ~ This function traverses the filesystem tree starting at a leaf pointed to by the 'current' argument until an empty branch pointer corresponding to the given key is found. ~ */

fsp fs_empty_branch_for_key(fsp _branch, fsp current, uint16_t key) {

	/* ~ If our 'current' pointer is zero, then an empty branch pointer has been found; return it. ~ */

	if (current == 0) return _branch;

	/* ~ Dereference the 'current' pointer to bring a copy of the leaf that it points to into local memory. ~ */

	leaf *_current = at45_dereference(current, sizeof(leaf));

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

		fsp region = at45_alloc(sizeof(leaf));
		if (!region) {
			error_raise(E_NO_MEM, "");
			return 0;
		}

		/* ~ Create the new leaf. ~ */

		leaf *_leaf = malloc(sizeof(leaf));
		if(!_leaf) {
			error_raise(E_NO_MEM, "");
			return 0;
		}

		/* ~ Clear the new leaf. ~ */

		memset(_leaf, 0, sizeof(leaf));

		/* ~ Index the leaf in the tree by overwriting by writing the newly allocated memory region to the empty branch pointer we found earlier. ~ */

		at45_push(&region, sizeof(fsp), _branch);

		/* ~ Associate the given key with the new leaf by writing the key given into the key property of the new leaf. ~ */

		_leaf -> key = key;

		/* ~ Write the empty branch pointer we found earlier into the branch pointer property of the new leaf. ~ */

		_leaf -> _branch = _branch;

		/* ~ Send the new leaf to the filesystem. ~ */

		at45_push(_leaf, sizeof(leaf), region);

		/* ~ Free memory allocated for the leaf. ~ */

		free(_leaf);

		/* ~ Return a pointer to the newly created leaf. ~ */

		return region;
	}

	else {

		/* ~ This case catches an exception wherein the leaf we want to add already exists. ~ */

		verbose("");

	}

	return 0;
}

/* ~ This function traverses the filesystem tree until a leaf matching a given key is found. ~ */

fsp fs_leaf_for_key(fsp current, uint16_t key) {

	/* ~ If we reach the end of the tree and have not yet found a matching key, then the leaf we are interested in finding does not exist. ~ */

	if (current == 0) {

		error_raise(E_FS_NO_LEAF, "");

		return 0;

	}

	/* ~ However, if we have not yet reached the end of the tree, dereference the 'current' leaf pointer to bring a copy of it into local memory. ~ */

	leaf *_current = at45_dereference(current, sizeof(leaf));

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

	if (match == 0) {

		verbose("");

		return;

	}

	/* ~ Otherwise, dereference it to bring a copy of it into local memory. ~ */

	leaf *_match = at45_dereference(match, sizeof(leaf));

	/* ~ Check the worst case scenario first; the leaf we want to delete has two children. Ugh. ~ */

	if (_match -> left && _match -> right) {

		verbose("");

		/* ~ De-index the leaf by replacing its branch pointer with a pointer to its left child. ~ */

		at45_push(&(_match -> left), sizeof(fsp), _match -> _branch);

		/* ~ Change the branch pointer of the leaf's left child to match its new index in the filesystem tree. ~ */

		at45_push(&(_match -> _branch), sizeof(fsp), forward(_match -> left, leaf, _branch));

		/* ~ Walk the left child of the leaf to find an empty branch pointer to which the leaf's orphaned right child can be appended. ~ */

		uint16_t *key = (uint16_t *) at45_dereference(forward(_match -> right, leaf, key), sizeof(uint16_t));

		fsp *right = (fsp *) at45_dereference(forward(_match -> left, leaf, right), sizeof(fsp));

		fsp empty = fs_empty_branch_for_key(forward(_match -> left, leaf, right), *right, *key);

		/* ~ Re-index the orphaned right child by writing its address into the empty branch pointer we found. ~ */

		at45_push(&(_match -> right), sizeof(fsp), empty);

		/* ~ Overwrite the branch pointer of the leaf's right child to reflect its new index in the filesystem tree. ~ */

		at45_push(&empty, sizeof(fsp), forward(_match -> right, leaf, _branch));

	}

	/* ~ Not a bad scenario; the leaf we wish to delete only has a left child. ~ */

	else if (_match -> left) {

		verbose("");

		/* ~ De-index the leaf by replacing its branch pointer with a pointer to its left child. ~ */

		at45_push(&(_match -> left), sizeof(fsp), _match -> _branch);

		/* ~ Change the branch pointer of the leaf's left child to match its new index in the filesystem tree. ~ */

		at45_push(&(_match -> _branch), sizeof(fsp), forward(_match -> left, leaf, _branch));

	}

	/* ~ Also not a bad scenario; the leaf we wish to delete only has a right child. ~ */

	else if (_match -> right) {

		verbose("");

		/* ~ De-index the leaf by replacing its branch pointer with a pointer to its right child. ~ */

		at45_push(&(_match -> right), sizeof(fsp), _match -> _branch);

		/* ~ Change the branch pointer of the leaf's right child to match its new index in the filesystem tree. ~ */

		at45_push(&(_match -> _branch), sizeof(fsp), forward(_match -> right, leaf, _branch));

	}

	else {

		/* ~ Best case scenario, the leaf to be deleted has no children. ~ */

		verbose("");

		/* ~ De-index the leaf from the tree by clearing its branch pointer. ~ */

		fsp zero = 0;

		at45_push(&zero, sizeof(fsp), _match -> _branch);

	}

	/* ~ Free any memory allocated to index the leaf. ~ */

	at45_free(match);

	/* ~ Free the memory allocated to store the local copy of the match. ~ */

	free(_match);

}
