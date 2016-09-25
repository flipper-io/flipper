#define __private_include__
#include <private/nvm.h>
#include <platform/fvm.h>
#include <flipper/fmr.h>

/* Pointer to the start of virtual non-volatile memory. */
uint8_t *v_nvm;

const struct _lf_endpoint lf_fvm_ep = {
	fvm_configure,
	fvm_ready,
	fvm_put,
	fvm_get,
	fvm_push,
	fvm_pull,
	fvm_destroy
};

void fvm_begin(void) {
	/* Allocate the memory needed to emulate NVM. */
	v_nvm = (uint8_t *)malloc(NVM_SIZE);
	/* Format NVM. */
	nvm_format();
}

void fvm_end(void) {
	/* Free the memory used to emulate virtual NVM. */
	free(v_nvm);
}

int fvm_configure(struct _lf_endpoint *endpoint) {
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
	struct _fmr_packet packet;
	memcpy(&packet, source, length);
#ifdef __lf_debug__
	lf_debug_packet(&packet);
#endif
	return lf_success;
}

int fvm_pull(void *destination, lf_size_t length) {
	memset(destination, 0, length);
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
