#define __private_include__
#include <private/nvm.h>
#include <platform/fvm.h>
#include <flipper/fmr.h>

/* Pointer to the start of virtual non-volatile memory. */
uint8_t *v_nvm;
/* Declaration of the virtual packet. */
struct _fmr_packet vpacket;

const struct _lf_endpoint lf_fvm_ep = {
	fvm_configure,
	fvm_ready,
	fvm_put,
	fvm_get,
	fvm_push,
	fvm_pull,
	fvm_destroy
};

int fvm_configure(struct _lf_endpoint *endpoint) {
	/* Allocate the memory needed to emulate NVM. */
	v_nvm = (uint8_t *)malloc(NVM_SIZE);
	/* Format NVM. */
	nvm_format();
	return 0;
}

uint8_t fvm_ready(void) {
	return 0;
}

void fvm_put(uint8_t byte) {

}

uint8_t fvm_get(void) {
	return 0;
}

int fvm_push(void *source, lf_size_t length) {
	memset(&vpacket, 0, sizeof(struct _fmr_packet));
	memcpy(&vpacket, source, length);
#ifdef __lf_debug__
	lf_debug_packet(&vpacket);
#endif
	struct _fmr_result result;
	error_pause();
	fmr_perform(&vpacket, &result);
	error_resume();
	memcpy(&vpacket, &result, sizeof(struct _fmr_result));
	return lf_success;
}

int fvm_pull(void *destination, lf_size_t length) {
	memcpy(destination, &vpacket, length);
	return lf_success;
}

int fvm_destroy(struct _lf_endpoint *endpoint) {
	if (v_nvm) {
		printf("FVM was destroyed successfully.\n");
		free(v_nvm);
		v_nvm = NULL;
	}
	return lf_success;
}
