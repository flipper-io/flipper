#define __private_include__
#include <flipper/fs.h>
#include <flipper/error.h>

/* All other FS related symbols are delared in 'osmium/fs/fs.c'. */

int fs_transfer(char *path, char *name) {
	/* Open the file for reading. */
	FILE *file = fopen(path, "rb");
	if (!file) {
		error_raise(E_FS_NO_FILE, error_message("Failed to open the file '%s' for reading.", path));
		return lf_error;
	}
	/* Obtain the size of the file. */
	fseek(file, 0L, SEEK_END);
	uint32_t size = (uint32_t)(ftell(file));
	fseek(file, 0L, SEEK_SET);
	/* Allocate the memory required to load the file into memory. */
	uint8_t *data = (uint8_t *) malloc(sizeof(uint8_t) * size);
	if (!data) {
		error_raise(E_MALLOC, error_message("Failed to obtain the memory required to transfer the file '%s'.", name));
		return lf_error;
	}
	/* Read the file into memory. */
	fread(data, size, sizeof(uint8_t), file);
	/* Create the file on the device. */
	int _e = fs_create(name, size);
	if (_e < lf_success) {
		return _e;
	}
	/* Push the data into the file. */
	fs_push(data, size);
	/* Free the memory allocated to load the file. */
	free(data);
	/* Close the file. */
	fclose(file);
	return lf_success;
}

int fs_receive(char *name, char *path) {
	/* Open the file for writing. */
	FILE *file = fopen (path, "wb");
	if (!file) {
		error_raise(E_FS_NO_FILE, error_message("Failed to create the file '%s'.", path));
		return lf_error;
	}
	lf_size_t size = 0;
	/* Allocate the memory required to download the file. */
	uint8_t *data = (uint8_t *) malloc(sizeof(uint8_t) * size);
	if (!data) {
		error_raise(E_MALLOC, error_message("Failed to obtain the memory required to receive the file '%s'.", name));
		return lf_error;
	}
	/* Pull the data from the file. */
	fs_push(data, size);
	/* Write the data into the filesystem. */
	fwrite(data, sizeof(uint8_t), size, file);
	/* Free the memory allocated to load the file. */
	free(data);
	/* Close the file. */
	fclose(file);
	return lf_success;
}
