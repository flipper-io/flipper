#define __private_include__
#include <flipper/platforms/posix.h>
#include <flipper/carbon/modules.h>

const void *const fmr_modules[] = {};

uint32_t fmr_call(const void *function, uint8_t argc, uint16_t argt, void *argv) {
	return 0;
}

/* TEMP */

void fmr_push(struct _fmr_push_pull_packet *packet) {
	
}

void fmr_pull(struct _fmr_push_pull_packet *packet) {

}
