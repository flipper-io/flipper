#define __private_include__
#include <flipper/carbon.h>
#include <flipper/posix/posix.h>

const void *const fmr_modules[] = {};

uint32_t fmr_call(const void *function, uint8_t argc, uint16_t argt, void *argv) {
	return 0;
}

fmr_return fmr_push(struct _fmr_push_pull_packet *packet) {
	return -1;
}

fmr_return fmr_pull(struct _fmr_push_pull_packet *packet) {
	return -1;
}
