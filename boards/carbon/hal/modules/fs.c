#define __private_include__
#include <flipper/flipper.h>
#include <flipper/carbon/fs.h>

/* NOTE: All functions in this module are declared as weak so they can be overwritten in the presence of FVM. */

int __attribute__((weak)) fs_configure(void) {
	return lf_success;
}

int __attribute__((weak)) fs_create(char *name, lf_size_t size) {
	return lf_success;
}

int __attribute__((weak)) fs_delete(char *name) {
	return lf_success;
}

int __attribute__((weak)) fs_open(char *name, lf_size_t offset) {
	return lf_success;
}

lf_size_t __attribute__((weak)) fs_size(void) {
	return 0;
}

void __attribute__((weak)) fs_seek(lf_size_t offset) {

}

uint8_t __attribute__((weak)) fs_get(void) {
	return 0;
}

void __attribute__((weak)) fs_push(void *source, lf_size_t length) {

}

void __attribute__((weak)) fs_pull(void *destination, lf_size_t length) {

}

void __attribute__((weak)) fs_close(void) {

}

void __attribute__((weak)) fs_format(void) {

}
