#include <flipper.h>

int fs_configure(void) {
	printf("Configuring the filesystem.\n");
	return lf_success;
}

int fs_create(char *name, lf_size_t size) {
	printf("Creating a file in the filesystem.\n");
	return lf_success;
}

int fs_delete(char *name) {
	printf("Deleting a file from the filesystem.\n");
	return lf_success;
}

int fs_open(char *name, lf_size_t offset) {
	printf("Opening a file from the filesystem.\n");
	return lf_success;
}

lf_size_t fs_size(void) {
	printf("Getting the size of the open file.\n");
	return 0;
}

void fs_seek(lf_size_t offset) {
	printf("Seeking in the open file.\n");
}

uint8_t fs_get(void) {
	printf("Getting a byte from the open file.\n");
	return 0;
}

void fs_push(void *source, lf_size_t length) {
	printf("Pushing data to the open file.\n");
}

void fs_pull(void *destination, lf_size_t length) {
	printf("Pulling data from the open file.\n");
}

void fs_close(void) {
	printf("Closing the open file.\n");
}

void fs_format(void) {
	printf("Formatting the filesystem.\n");
}
