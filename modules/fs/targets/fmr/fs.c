#define __private_include__
#include <flipper/flipper.h>
#include <flipper/modules.h>

/* ~ Provide the definition for this standard module. ~ */
LF_MODULE(_fs, "fs", "Provides access to the device's filesystem.", _fs_id);

int fs_configure(void) {
	return lf_invoke(&_fs, _fs_configure, NULL);
}

void fs_format(void) {
	lf_invoke(&_fs, _fs_format, NULL);
}

int fs_create(char *name, void *data, size_t length) {
	return lf_success;
}

int fs_remove(char *name) {
	return lf_invoke(&_fs, _fs_remove, NULL);
}

int fs_rename(char *from, char *to) {
	return lf_invoke(&_fs, _fs_rename, NULL);
}

void fs_write(char *name) {
	lf_invoke(&_fs, _fs_write, NULL);
}

/* fs.put maps to nvm_put */

void fs_read(char *name) {
	lf_invoke(&_fs, _fs_read, NULL);
}

/* fs.get maps to nvm_get */

nvm_p fs_data(char *name) {
	return lf_invoke(&_fs, _fs_data, NULL);
}
