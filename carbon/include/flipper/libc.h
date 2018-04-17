#ifndef __libc_h__
#define __libc_h__

#include <flipper/types.h>

void *libc_malloc(size_t length);
void libc_free(void *ptr);
void *libc_memcpy(void *dst, void *src, size_t n);

#endif
