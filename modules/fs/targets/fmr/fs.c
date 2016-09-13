#define __private_include__
#include <flipper/fs.h>
#include <flipper/fmr.h>

int fs_configure(void) {
    return lf_error;
}

void fs_format(void) {

}

int fs_create(char *name, void *data, size_t length) {
    return lf_error;
}

int fs_remove(char *name) {
    return lf_error;
}

int fs_rename(char *from, char *to) {
    return lf_error;
}

void fs_write(char *name) {

}

/* fs.put maps to nvm_put */

void fs_read(char *name) {

}

/* fs.get maps to nvm_get */

nvm_p fs_data(char *name) {
    return 0;
}
