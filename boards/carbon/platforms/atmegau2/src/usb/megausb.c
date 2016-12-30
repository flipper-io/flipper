#define __private_include__
#include <megausb.h>
#include <flipper/carbon/platforms/atmega16u2.h>

struct _lf_endpoint megausb = {
	megausb_configure,
	megausb_ready,
	megausb_put,
	megausb_get,
	megausb_push,
	megausb_pull,
	megausb_destroy
};

int megausb_configure(void) {
	return lf_success;
}

uint8_t megausb_ready(struct _lf_endpoint *this) {
	return 0;
}

void megausb_put(struct _lf_endpoint *this, uint8_t byte) {

}

uint8_t megausb_get(struct _lf_endpoint *this) {
	return 0;
}

int megausb_push(struct _lf_endpoint *this, void *source, lf_size_t length) {
	int8_t bytes;
#ifndef __ALL_BULK__
	if (length <= INTERRUPT_IN_SIZE) {
		bytes = megausb_interrupt_transmit(source, length);
	} else {
#endif
		bytes = megausb_bulk_transmit(source, length);
#ifndef __ALL_BULK__
	}
#endif
	if (bytes <= 0) {
		return lf_error;
	}
	return lf_success;
}

int megausb_pull(struct _lf_endpoint *this, void *destination, lf_size_t length) {
	int8_t bytes;
#ifndef __ALL_BULK__
	if (length <= INTERRUPT_OUT_SIZE) {
		bytes = megausb_interrupt_receive(destination, length);
	} else {
#endif
		bytes = megausb_bulk_receive(destination, length);
#ifndef __ALL_BULK__
	}
#endif
	if (bytes <= 0) {
		return lf_error;
	}
	return 	lf_success;
}

int megausb_destroy(void) {
	return lf_success;
}
