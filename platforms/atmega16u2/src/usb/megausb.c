#define __private_include__
#include <private/megausb.h>
#include <platform/atmega16u2.h>

const struct _lf_endpoint megausb = {
	megausb_configure,
	megausb_ready,
	megausb_put,
	megausb_get,
	megausb_push,
	megausb_pull,
	megausb_destroy
};

int megausb_configure(struct _lf_endpoint *endpoint) {
	return lf_success;
}

uint8_t megausb_ready(void) {
	return 0;
}

void megausb_put(uint8_t byte) {

}

uint8_t megausb_get(void) {
    return 0;
}

int megausb_push(void *source, lf_size_t length) {
	for (lf_size_t packet = 0; packet < lf_ceiling(length, INTERRUPT_IN_SIZE); packet ++) {
		uint8_t len = INTERRUPT_IN_SIZE;
		if (length < len) {
			len = length;
		}
		int8_t bytes = megausb_interrupt_transmit((uint8_t *)(source + (packet * INTERRUPT_IN_SIZE)), len);
		if (bytes <= 0) {
			return lf_error;
		}
		length -= len;
	}
	return lf_success;
}

int megausb_pull(void *destination, lf_size_t length) {
	for (lf_size_t packet = 0; packet < lf_ceiling(length, INTERRUPT_OUT_SIZE); packet ++) {
		uint8_t len = INTERRUPT_OUT_SIZE;
		if (length < len) {
			len = length;
		}
		int8_t bytes = megausb_interrupt_receive((uint8_t *)(destination + (packet * INTERRUPT_OUT_SIZE)), len);
		if (bytes <= 0) {
			return lf_error;
		}
		length -= len;
	}
	return 	lf_success;
}

int megausb_destroy(struct _lf_endpoint *endpoint) {
	return lf_success;
}
