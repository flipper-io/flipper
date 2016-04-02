#define __private_include__
#include <flipper/fs.h>
#include <flipper/fmr.h>
#include <flipper/error.h>
#include <flipper/at45.h>
#include <flipper/fs/crc.h>
#include <flipper/fs/tree.h>

#define FLIPPER_WANT_PROGRESS 2
#define FLIPPER_PUSH 1
#define FLIPPER_PULL 0
#define AT45_CHUNK_SIZE (64-22)

void fs_configure(void) {

	/* ~ In order to syncronize filesystem references, we have to load the freelist, break_value, and root_leaf references from external memory! ~ */
	at45_pull(&_free_list, sizeof(fsp), _FREE_LIST);
	at45_pull(&_break_value, sizeof(fsp), _BREAK_VALUE);
	at45_pull(&_root_leaf, sizeof(fsp), _ROOT_LEAF);

}

void fs_format(void) {

	device.invoke(_fs, _fs_format, NO_ARGS);

	fs_configure();

}

fsp fs_data(char *name) {

	uintcrc_t id = checksum(name, strlen(name));

	/* ~ Obtain a pointer to the file. ~ */
	fsp _leaf = fs_leaf_for_key(_root_leaf, id);

	/* ~ Ensure we have a valid file. ~ */
	if (!_leaf) return 0;

	/* ~ Pull in the address of the file's data. ~ */
	fsp data;
	at45_pull(&data, sizeof(fsp), forward(_leaf, leaf, data));

	return data;

}

void chunky_transfer(uint8_t *buffer, size_t size, fsp data, int flags) {

	unsigned int remaining_bytes = size;
	unsigned int read_idx = 0;

	while (remaining_bytes > 0) {

		int chunk_size = (remaining_bytes > AT45_CHUNK_SIZE) ? (AT45_CHUNK_SIZE) : (remaining_bytes);

		if (flags & FLIPPER_PUSH) {

			at45_push(buffer + read_idx, chunk_size, data + read_idx);

		}

		else {
			
			at45_pull(buffer + read_idx, chunk_size, data + read_idx);

		}

		if (flags & FLIPPER_WANT_PROGRESS) {

			// print_progress(read_idx, size, 60);

		}

		read_idx += chunk_size;
		remaining_bytes -= chunk_size;

	}
}

fsp fs_upload(char *path, char *name) {

	fsp _leaf;

	/* ~ Open the file for reading. ~ */
	FILE *file = fopen(path, "rb");
	if (!file) {
		error_raise(E_FS_OPEN, "Could not open file '%s'.", path);
		goto cleanup;
	}

	/* ~ Obtain the size of the file. ~ */
	fseek(file, 0L, SEEK_END);
	uint32_t size = (uint32_t)(ftell(file));
	fseek(file, 0L, SEEK_SET);

	/* ~ Load the file into RAM. ~ */
	uint8_t *binary = (uint8_t *) malloc(sizeof(uint8_t) * size);
	if(!binary) {
		error_raise(E_NO_MEM, "Failed to obtain the memory required to upload the file '%s'.", name);
	}

	fread(binary, size, sizeof(uint8_t), file);

	/* ~ Generate a key for the file by checksumming its name. ~ */
	uint16_t key = checksum(name, strlen(name));

	/* ~ Create a new leaf to hold the file metadata. ~ */
	_leaf = fs_add_leaf_with_key(_root_leaf, key);
	if (!_leaf) {
		goto cleanup;
	}

	/* ~ Allocate space for the file in external memory. ~ */
	fsp _data = at45_alloc(size);

	/* ~ Save the file size and pointer to the file data into the leaf. ~ */
	at45_push(&size, sizeof(fsp), forward(_leaf, leaf, size));
	at45_push(&_data, sizeof(fsp), forward(_leaf, leaf, data));

	/* ~ Move the data into external flash. ~ */
	chunky_transfer(binary, size, _data, FLIPPER_PUSH | FLIPPER_WANT_PROGRESS);

cleanup:

	/* ~ Clean up and close the file. ~ */
	free(binary);
	fclose(file);

	return _leaf;

}

void fs_download(char *name, char *path) {

	/* ~ Open the file for writing. ~ */
	FILE *file = fopen (path, "wb");
	if (!file) {
		error_raise(E_FS_OPEN, "Could not open file '%s' for writing.", path);
		return;
	}

	/* ~ Obtain the key for the file by c hecksumming its name. ~ */
	uint16_t key = checksum(name, strlen(name));

	/* ~ Locate the metadata for the file in the filesystem. ~ */
	fsp _leaf = fs_leaf_for_key(_root_leaf, key);
	if (!_leaf) {
		return;
	}

	/* ~ Load the metadata. ~ */
	leaf *l = at45_dereference(_leaf, sizeof(leaf));

	/* ~ Create a buffer to hold the data. ~ */
	uint8_t *binary = malloc(l -> size);

	/* ~ Retrieve the file in chunks. ~ */
	chunky_transfer(binary, l -> size, l -> data, FLIPPER_PULL | FLIPPER_WANT_PROGRESS);

	/* ~ Write the data to disk. ~ */
	fwrite(binary, l -> size, sizeof(uint8_t), file);

	/* ~ Clean up and close the file. ~ */
	free(l);
	free(binary);
	fclose(file);

}
