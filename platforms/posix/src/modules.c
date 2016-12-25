#define __private_include__
#include <platforms/posix.h>
#include <flipper/modules.h>

uint32_t fmr_call(const void *function, uint8_t argc, uint16_t argt, void *argv) {
	return 0;
}

/* TEMP */

void fmr_push(fmr_module module, fmr_function function, lf_size_t length) {
	void *swap = malloc(length);
	if (!swap) {
		error_raise(E_MALLOC, NULL);
	}

}

void fmr_pull(fmr_module module, fmr_function function, lf_size_t length) {

}
