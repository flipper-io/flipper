#include <flipper.h>

enum { _libc_memcpy, _libc_free, _libc_malloc };

void* libc_memcpy(void* dst, void* src, size_t n);
void libc_free(void* ptr);
void* libc_malloc(size_t length);

void *libc_interface[] = {
	&libc_memcpy,
	&libc_free,
	&libc_malloc
};

LF_MODULE(libc, "libc", libc_interface);

LF_WEAK void* libc_memcpy(void* dst, void* src, size_t n) {
	return lf_invoke(lf_get_current_device(), "libc", _libc_memcpy, lf_int32_t, lf_args(lf_infer(dst), lf_infer(src), lf_infer(n)));
}

LF_WEAK void libc_free(void* ptr) {
	lf_invoke(lf_get_current_device(), "libc", _libc_free, lf_void_t, lf_args(lf_infer(ptr)));
}

LF_WEAK void* libc_malloc(size_t length) {
	return lf_invoke(lf_get_current_device(), "libc", _libc_malloc, lf_int32_t, lf_args(lf_infer(length)));
}

