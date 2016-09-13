#define __private_include__
#include <flipper/fs.h>
#include <flipper/error.h>
#include <private/nvm.h>

/* All other FS related symbols are delared in 'osmium/fs/fs.c'. */

nvm_p fs_transfer(char *path, char *name) {
	/* Open the file for reading. */
	FILE *file = fopen(path, "rb");
	if (!file) {
		error_raise(E_FS_NO_FILE, error_message("Could not open the file '%s' for reading.", path));
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
		error_raise(E_FS_NO_FILE, error_message("Could not create local file '%s'.", path));
		return;
	}
	/* Obtain a key for the file given its name. */
	lf_id_t key = lf_checksum(name, strlen(name));

/* WARNING: ILLEGAL FS ACCESSORS HERE FROM LIBFLIPPER CONTEXT. */
#if 0
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
#endif

	/* Close the file. */
	fclose(file);
}
