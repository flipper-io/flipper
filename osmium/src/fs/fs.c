#define __private_include__
#include <flipper/fs.h>
#include <private/nvm.h>
#include <flipper/error.h>

int fs_configure(void) {
	return lf_success;
}

int fs_create(char *name, lf_size_t size) {
	/* Obtain a key for the file given its name. */
	lf_id_t key = lf_checksum(name, strlen(name));
	/* Create a leaf for the key. */
	nvm_p _leaf = fs_add_leaf_with_key(_root_leaf, key);
	if (!_leaf) {
		return lf_error;
	}
	/* Allocate space in external memory for the file's data. */
	nvm_p _data = nvm_alloc(size);
	if (!_data) {
		return lf_error;
	}
	/* Write the file metadata into the leaf. */
	nvm_push(&size, sizeof(lf_size_t), fs_access(_leaf, leaf, size));
	nvm_push(&_data, sizeof(nvm_p), fs_access(_leaf, leaf, data));
	return lf_success;
}

int fs_delete(char *name) {
	/* Obtain a key for the file given its name. */
	lf_id_t key = lf_checksum(name, strlen(name));
	/* Obtain the file's leaf, suppressing any errors that occur while doing so. */
	nvm_p _leaf = fs_leaf_for_key(_root_leaf, key);
	if (!_leaf) {
		return -1;
	}
	nvm_p _data;
	/* Read the pointer to the file's data into the variable above. */
	nvm_pull(&_data, sizeof(nvm_p), fs_access(_leaf, leaf, data));
	/* Free any external memory allocated to store the file's data. */
	if (_data) {
		nvm_free(_data);
	}
	/* Remove the file's leaf. */
	return fs_remove_leaf_with_key(_root_leaf, key);;
}

lf_size_t fs_size(char *name) {
	return 0;
}

int fs_open(char *name, lf_size_t offset) {
	/* Obtain a key for the file given its name. */
	lf_id_t key = lf_checksum(name, strlen(name));
	/* Obtain the file's leaf, suppressing any errors that occur while doing so. */
	nvm_p _leaf = fs_leaf_for_key(_root_leaf, key);
	if (!_leaf) {
		return lf_error;
	}
	nvm_p _data;
	nvm_pull(&_data, sizeof(nvm_p), fs_access(_leaf, leaf, data));
	_rw_head = _data + offset;
	return lf_success;
}

uint8_t fs_get(void){
	return nvm_get();
}

void fs_push(void *source, lf_size_t length) {
	nvm_push(source, length, _rw_head);
	_rw_head += length;
}

void fs_pull(void *destination, lf_size_t length) {
	nvm_pull(destination, length, _rw_head);
	_rw_head += length;
}

void fs_close(void) {
	/* Disable external memory to finish the operation. */
	nvm_disable();
}

void fs_format(void) {
	/* Reset the free list pointer. */
	_free_list = 0;
	/* Write the free list pointer to NVM. */
	nvm_push(&_free_list, sizeof(nvm_p), _FREE_LIST);
	/* Reset the break value pointer. */
	_break_value = nvm_address_from_page_and_offset(1, 0);
	/* Write the break value pointer to NVM. */
	nvm_push(&_break_value, sizeof(nvm_p), _BREAK_VALUE);
	/* ~ Create the root leaf. ~ */
	leaf root = { 0 };
	/* Set the key of the root leaf. */
	root.key = 0x4321;
	/* ~ Allocate space externally for the root leaf. ~ */
	_root_leaf = nvm_alloc(sizeof(leaf));
	if (!_root_leaf) {
		error_raise(E_MALLOC, error_message("The request for external memory was denied when trying to format the filesystem."));
	}
	/* Write the root leaf pointer to NVM. */
	nvm_push(&_root_leaf, sizeof(nvm_p), _ROOT_LEAF);
	/* Write the root leaf into NVM. */
	nvm_push(&root, sizeof(leaf), _root_leaf);
}
