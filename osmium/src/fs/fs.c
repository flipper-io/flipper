#define __private_include__
#include <flipper/fs.h>
#include <private/nvm.h>
#include <flipper/error.h>

int fs_configure(void) {
	return lf_success;
}

int fs_create(char *name) {
	/* Obtain a key for the file given its name. */
	lf_id_t key = lf_checksum(name, strlen(name));
	/* Create a leaf for the key. */
	suppress_errors(nvm_p _leaf = fs_add_leaf_with_key(_root_leaf, key));
	if (!_leaf) {
		/* Raise an error with the error code generated from the statement above. */
		error_raise(error_get(), error_message("Failed to create file named '%s'.", name));
		return lf_error;
	}
	// /* If there is data to copy, perform the necessary actions. */
	// if (data && length) {
	// 	/* Ensure that the length is valid. */
	// 	if (length > LF_SIZE_T_MAX) {
	// 		error_raise(E_OVERFLOW, "The length provided to the function 'fs_create' exceeds the maximum value FMR is able to convey.");
	// 		return lf_error;
	// 	}
	// 	/* Write the size of the data represented by the leaf. */
	// 	nvm_push(&length, sizeof(uint32_t), lf_forward(_leaf, leaf, size));
	// 	/* Allocate the external memory required to hold the file's data. */
	// 	nvm_p _data = nvm_alloc((lf_size_t)(length));
	// 	if (!_data) {
	// 		error_raise(E_MALLOC, error_message("Could not allocate the memory required to save the file '%s'.", name));
	// 	}
	// 	/* Copy the file's data into external memory. */
	// 	nvm_push(data, (lf_size_t)(length), _data);
	// 	/* Write the file's data pointer into the leaf. */
	// 	nvm_push(&_data, sizeof(nvm_p), lf_forward(_leaf, leaf, data));
	// }
	return lf_success;
}

int fs_delete(char *name) {
	/* Obtain a key for the file given its name. */
	lf_id_t key = lf_checksum(name, strlen(name));
	/* Obtain the file's leaf, suppressing any errors that occur while doing so. */
	suppress_errors(nvm_p _leaf = fs_leaf_for_key(_root_leaf, key));
	if (!_leaf) {
		error_raise(E_FS_NO_FILE, error_message("Failed to remove file '%s'.", name));
		return -1;
	}
	/* Create a locally scoped variable into which we can bring the pointer. */
	nvm_p _data;
	/* Read the pointer to the file's data into the variable above. */
	nvm_pull(&_data, sizeof(nvm_p), lf_forward(_leaf, leaf, data));
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

void fs_write(char *name, lf_size_t offset) {

}

void fs_put(uint8_t byte){

}

void fs_read(char *name, lf_size_t offset) {

}

uint8_t fs_get(void){
	return 0;
}

void fs_push(void *source, lf_size_t length) {

}

void fs_pull(void *destination, lf_size_t length) {

}

void fs_close(void){

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
	/* ~ Allocate space externall for the root leaf. ~ */
	_root_leaf = nvm_alloc(sizeof(leaf));
	if (!_root_leaf) {
		error_raise(E_MALLOC, error_message("The request for external memory was denied when trying to format the filesystem."));
	}
	/* Write the root leaf pointer to NVM. */
	nvm_push(&_root_leaf, sizeof(nvm_p), _ROOT_LEAF);
	/* Write the root leaf into NVM. */
	nvm_push(&root, sizeof(leaf), _root_leaf);
}
