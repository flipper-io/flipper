/* This libc module provides RPC access to libc functions. */

#include <flipper.h>

LF_FUNC("libc") void *libc_malloc(size_t length) {
    return malloc(length);
}

LF_FUNC("libc") void libc_free(void *ptr) {
    free(ptr);
}

LF_FUNC("libc") void *libc_memcpy(void *dst, void *src, size_t n) {
    return memcpy(dst, src, n);
}
