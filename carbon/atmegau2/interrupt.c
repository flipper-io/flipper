#define __private_include__
#include <flipper/atmegau2/megausb.h>

/* Receive a packet using the appropriate interrupt endpoint. */
int8_t megausb_interrupt_receive(uint8_t *destination, lf_size_t length) {

	/* If USB is not configured, return with error. */
	if (!megausb_configured) {
		return lf_error;
	}

	/* Select the endpoint that has been configured to receive interrupt data. */
	UENUM = INTERRUPT_OUT_ENDPOINT;

	int total = lf_ceiling(length, INTERRUPT_OUT_SIZE);
	for (int i = 0; i < total; i ++) {

		/* Wait until the USB controller is ready. */
		int _e = megausb_wait_ready();
		if (_e < lf_success) {
			return lf_error;
		}

		/* Transfer the buffered data to the destination. */
		uint8_t len = INTERRUPT_OUT_SIZE;
		while (len --) {
			if (length --) {
				/* If there is still valid data to send, load it from the receive buffer. */
				*destination ++ = UEDATX;
			} else {
				/* Otherwise, flush the buffer. */
				break;
			}
		}

		/* Re-enable interrupts for the receive endpoint. */
		UEINTX = (1 << NAKINI) | (1 << RWAL) | (1 << RXSTPI) | (1 << STALLEDI) | (1 << TXINI);
	}

	return lf_success;
}

/* Send a packet using the appropriate interrupt endpoint. */
int8_t megausb_interrupt_transmit(uint8_t *source, lf_size_t length) {

	/* If USB is not configured, return with error. */
	if (!megausb_configured) {
		return lf_error;
	}

	/* Select the endpoint that has been configured to transmit interrupt data. */
	UENUM = INTERRUPT_IN_ENDPOINT & ~USB_IN_MASK;

	int total = lf_ceiling(length, INTERRUPT_IN_SIZE);
	for (int i = 0; i < total; i ++) {

		/* Wait until the USB controller is ready. */
		int _e = megausb_wait_ready();
		if (_e < lf_success) {
			return lf_error;
		}

		/* Transfer the buffered data to the destination. */
		uint8_t len = INTERRUPT_IN_SIZE;
		while (len --) {
			if (length --) {
				UEDATX = *source ++;
			} else {
				/* Otherwise, flush the buffer. */
				break;
			}
		}

		/* Re-enable interrupts for the transmit endpoint. */
		UEINTX = (1 << RWAL) | (1 << NAKOUTI) | (1 << RXSTPI) | (1 << STALLEDI);
	}

	return lf_success;
}
