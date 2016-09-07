#define __private_include__
#include <flipper/fs.h>
#include <private/nvm.h>
#include <flipper/error.h>

/* Define the virtual interface for this module. */
const struct _fs fs = {
	fs_configure,
	fs_format,
	fs_create,
	fs_remove,
	fs_rename,
	fs_write,
	nvm_put,
	fs_read,
	nvm_get,
	fs_data,
	fs_transfer,
	fs_receive
};

int fs_configure(void) {
	return lf_success;
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
	leaf root;
	/* Zero the leaf. */
	memset(&root, 0, sizeof(leaf));
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

int fs_create(char *name, void *data, size_t length) {
	/* Obtain a key for the file given its name. */
	lf_id_t key = lf_checksum(name, strlen(name));
	/* Create a leaf for the key. */
	suppress_errors(nvm_p _leaf = fs_add_leaf_with_key(_root_leaf, key));
	if (!_leaf) {
		/* Raise an error with the error code generated from the statement above. */
		error_raise(E_LAST, error_message("Failed to create file named '%s'.", name));
		return lf_error;
	}
	/* If there is data to copy, perform the necessary actions. */
	if (data && length) {
		/* Ensure that the length is valid. */
		if (length > LF_SIZE_T_MAX) {
			error_raise(E_OVERFLOW, "The length provided to the function 'fs_create' exceeds the maximum value FMR is able to convey.");
			return lf_error;
		}
		/* Write the size of the data represented by the leaf. */
		nvm_push(&length, sizeof(uint32_t), lf_forward(_leaf, leaf, size));
		/* Allocate the external memory required to hold the file's data. */
		nvm_p _data = nvm_alloc((lf_size_t)(length));
		if (!_data) {
			error_raise(E_MALLOC, error_message("Could not allocate the memory required to save the file '%s'.", name));
		}
		/* Copy the file's data into external memory. */
		nvm_push(data, (lf_size_t)(length), _data);
		/* Write the file's data pointer into the leaf. */
		nvm_push(&_data, sizeof(nvm_p), lf_forward(_leaf, leaf, data));
	}
	/* Return with success. */
	return 0;
}

int fs_remove(char *name) {
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

int fs_rename(char *from, char *to) {
	/* Obtain a key for the file given its name. */
	lf_id_t from_key = lf_checksum(from, strlen(from));
	/* Obtain the file's leaf. */
	suppress_errors(nvm_p _from_leaf = fs_leaf_for_key(_root_leaf, from_key));
	if (!_from_leaf) {
		error_raise(E_FS_NO_FILE, error_message("Failed to rename the file '%s'.", from));
		return lf_error;
	}
	/* Calculate the key for the file's new name. */
	lf_id_t to_key = lf_checksum(to, strlen(to));
	/* Create a leaf for the new key. */
	nvm_p _to_leaf = fs_add_leaf_with_key(_root_leaf, to_key);
	if (!_to_leaf) {
		return lf_error;
	}
	/* Copy BOTH the file's size and data pointers into the new leaf. */
	nvm_copy(lf_forward(_to_leaf, leaf, size), lf_forward(_from_leaf, leaf, size), sizeof(uint32_t) + sizeof(nvm_p));
	/* Remove the old leaf and return. */
	return fs_remove_leaf_with_key(_root_leaf, from_key);
}

void fs_write(char *name) {

}

/* fs.put maps to nvm_put */

void fs_read(char *name) {

}

/* fs.get maps to nvm_get */

nvm_p fs_data(char *name) {
	/* Obtain a key for the file given its name. */
	lf_id_t key = lf_checksum(name, strlen(name));
	/* Obtain the file's leaf, suppressing any errors that occur while doing so. */
	suppress_errors(nvm_p _leaf = fs_leaf_for_key(_root_leaf, key));
	if (!_leaf) {
		error_raise(E_FS_NO_FILE, error_message("Cannot get data from file named '%s'.", name));
		return 0;
	}
	/* Create a locally scoped variable into which we can bring the pointer. */
	nvm_p _data;
	/* Read the pointer to the file's data into the variable above. */
	nvm_pull(&_data, sizeof(nvm_p), lf_forward(_leaf, leaf, data));
	/* Return the pointer to the file's data. */
	return _data;
}

nvm_p fs_transfer(char *path, char *name) {
	/* Open the file for reading. */
	FILE *file = fopen(path, "rb");
	if (!file) {
		error_raise(E_FS_NO_FILE, "Could not open the file '%s' for reading.", path);
		return 0;
	}
	/* Obtain the size of the file. */
	fseek(file, 0L, SEEK_END);
	uint32_t size = (uint32_t)(ftell(file));
	fseek(file, 0L, SEEK_SET);
	/* Allocate the memory required to load the file into memory. */
	uint8_t *data = (uint8_t *) malloc(sizeof(uint8_t) * size);
	if (!data) {
		error_raise(E_MALLOC, error_message("Failed to obtain the memory required to upload the file '%s'.", name));
		return 0;
	}
	/* Read the file into memory. */
	fread(data, size, sizeof(uint8_t), file);
	/* Create the file on the device and load the data. */
	nvm_p _leaf = fs_create(name, data, size);
	/* Free the memory allocated to load the file. */
	free(data);
	/* Close the file. */
	fclose(file);
	/* Return a pointer to the file. */
	return _leaf;
}

void fs_receive(char *name, char *path) {
	/* Open the file for writing. */
	FILE *file = fopen (path, "wb");
	if (!file) {
		error_raise(E_FS_NO_FILE, "Could not create local file '%s'.", path);
		return;
	}
	/* Obtain a key for the file given its name. */
	lf_id_t key = lf_checksum(name, strlen(name));
	/* Obtain the file's leaf, suppressing any errors that occur while doing so. */
	suppress_errors(nvm_p _leaf = fs_leaf_for_key(_root_leaf, key));
	if (!_leaf) {
		error_raise(E_FS_NO_FILE, error_message("Failed to receive the file '%s'.", name));
		return;
	}
	/* Create a locally scoped variable into which we can bring the data pointer. */
	nvm_p size;
	/* Read the data pointer to the file's data into the variable above. */
	nvm_pull(&size, sizeof(nvm_p), lf_forward(_leaf, leaf, size));
	/* Create a locally scoped variable into which we can bring in the data pointer. */
	nvm_p _data;
	/* Read the data pointer to the file's data into the variable above. */
	nvm_pull(&_data, sizeof(nvm_p), lf_forward(_leaf, leaf, data));
	/* Load the file's data into memory. */
	uint8_t *data = nvm_dereference(_data, size);
	/* Write the data to disk. */
	fwrite(data, size, sizeof(uint8_t), file);
	/* Free the memory allocated to load the file. */
	free(data);
	/* Close the file. */
	fclose(file);
}