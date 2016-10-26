#define __private_include__
#include <flipper/flipper.h>
#include <flipper/modules.h>

/* ~ Provide the definition for this standard module. ~ */
LF_MODULE(_fs, "fs", "Provides access to the device's filesystem.", _fs_id);

int fs_configure(void) {
	return lf_success;
}

int fs_create(char *name, lf_size_t size) {
	return lf_success;
}

int fs_delete(char *name) {
	return lf_success;
}

lf_size_t fs_size(char *name) {
	return 0;
}

int fs_open(char *name, lf_size_t offset) {
	return lf_success;
}

uint8_t fs_get(void) {
	return 0;
}

void fs_push(void *source, lf_size_t length) {

}

void fs_pull(void *destination, lf_size_t length) {

}

void fs_close(void) {

}

void fs_format(void) {

}
