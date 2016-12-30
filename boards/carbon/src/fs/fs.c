#define __private_include__
#include <flipper/carbon/fs.h>
#include <nvm.h>
#include <flipper/carbon/error.h>

/* Declare all of the global variables for this module. */
nvm_p _free_list;
nvm_p _break_value;
nvm_p _root_leaf;
nvm_p _rw_head;
nvm_p _open_data;
lf_size_t open_size;

int fs_configure(void) {
	return lf_success;
}

int fs_create(char *name, lf_size_t size) {
	/* Obtain a key for the file given its name. */
	lf_crc_t key = lf_crc(name, strlen(name));
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
	lf_crc_t key = lf_crc(name, strlen(name));
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

int fs_open(char *name, lf_size_t offset) {
	/* Obtain a key for the file given its name. */
	lf_crc_t key = lf_crc(name, strlen(name));
	/* Obtain the file's leaf, suppressing any errors that occur while doing so. */
	nvm_p _leaf = fs_leaf_for_key(_root_leaf, key);
	if (!_leaf) {
		return lf_error;
	}
	nvm_pull(&open_size, sizeof(lf_size_t), fs_access(_leaf, leaf, size));
	nvm_pull(&_open_data, sizeof(nvm_p), fs_access(_leaf, leaf, data));
	_rw_head = _open_data + offset;
	return lf_success;
}

lf_size_t fs_size(void) {
	return open_size;
}

void fs_seek(lf_size_t offset) {
	_rw_head = _open_data + offset;
	if (_rw_head > _open_data + open_size) {
		error_raise(E_BOUNDARY, error_message("Seeking out of bounds."));
	}
}

uint8_t fs_get(void){
	uint8_t c = nvm_get();
	_rw_head ++;
	return c;
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
	_rw_head = 0;
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
