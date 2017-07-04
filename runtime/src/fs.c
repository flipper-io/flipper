#include <flipper/libflipper.h>

#ifdef __use_fs__
#define __private_include__
#include <flipper/fs.h>

LF_MODULE(_fs, "fs", "Provides access to the device's filesystem.");

/* Define the virtual interface for this module. */
const struct _fs fs = {
	fs_configure,
	fs_create,
	fs_delete,
	fs_open,
	fs_size,
	fs_seek,
	fs_get,
	fs_push,
	fs_pull,
	fs_close,
	fs_format,
	#ifdef __fs_transfer_symbols__
	fs_transfer,
	fs_receive
	#endif
};

LF_WEAK int fs_configure(void) {
	return lf_success;
}

LF_WEAK int fs_create(char *name, lf_size_t size) {
	return lf_success;
}

LF_WEAK int fs_delete(char *name) {
	return lf_success;
}

LF_WEAK int fs_open(char *name, lf_size_t offset) {
	return lf_success;
}

LF_WEAK lf_size_t fs_size(void) {
	return 0;
}

LF_WEAK void fs_seek(lf_size_t offset) {

}

LF_WEAK uint8_t fs_get(void) {
	return 0;
}

LF_WEAK void fs_push(void *source, lf_size_t length) {

}

LF_WEAK void fs_pull(void *destination, lf_size_t length) {

}

LF_WEAK void fs_close(void) {

}

LF_WEAK void fs_format(void) {

}

#endif
