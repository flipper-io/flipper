#include <flipper.h>

LF_WEAK lf_return_t fmr_call(lf_return_t (* function)(void), lf_type ret, uint8_t argc, uint16_t argt, void *argv) {
	return -1;
}

LF_WEAK lf_return_t fmr_push(struct _fmr_push_pull_packet *packet) {
	return -1;
}

LF_WEAK lf_return_t fmr_pull(struct _fmr_push_pull_packet *packet) {
	return -1;
}

LF_WEAK lf_return_t fmr_dyld(struct _fmr_dyld_packet *packet) {
	return -1;
}
