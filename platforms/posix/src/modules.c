#define __private_include__
#include <flipper/fmr.h>
#include <flipper/modules.h>
#include <platform/posix.h>

/* Define the standard modules accessible on this platform. */
const void *const fmr_modules[] = {
    _forward_id,
    &button,
    _forward_id,
    _forward_id,
    _forward_id,
    _forward_id,
    _forward_id,
    _forward_id,
    &led,
    _forward_id,
    _forward_id,
    _forward_id,
    _forward_id,
    _forward_id,
    _forward_id,
    _forward_id,
    _forward_id,
};

const void *lf_std_module(lf_id_t module) {
    return fmr_modules[module];
}

uint32_t fmr_call(void *function, uint8_t argc, uint16_t argt, void *argv) {
    return 0;
}

/* TEMP */

fmr_return fmr_push(fmr_module module, fmr_function function, lf_size_t length) {
	void *swap = malloc(length);
	if (!swap) {
		error_raise(E_MALLOC, NULL);
	}

}

void fmr_pull(uint16_t address, lf_size_t length) {

}
