#define __private_include__
#include <flipper/atmegau2/megausb.h>

/* Has bulk already timed out? */


/* Receive a packet using the appropriate bulk endpoint. */
int8_t megausb_bulk_receive(uint8_t *destination, lf_size_t length) {

	/* If USB is not configured, return with error. */
	if (!megausb_configured) {
		return lf_error;
	}

	/* Select the endpoint that has been configured to receive bulk data. */
	UENUM = BULK_OUT_ENDPOINT;

	int total = lf_ceiling(length, BULK_OUT_SIZE);
	for (int i = 0; i < total; i ++) {

		/* Wait until the USB controller is ready. */
		volatile uint8_t timeout = UDFNUML + LF_USB_TIMEOUT_MS;
		while (!(UEINTX & (1 << RWAL))) {
			if (!megausb_configured || UDFNUML == timeout) {
				/* Reset the endpoint hardware. */
				UERST = 0x1E;
        		UERST = 0;
				goto failure;
			}
		}

		/* Transfer the buffered data to the destination. */
		uint8_t len = BULK_OUT_SIZE;
		while (len --) {
			if (length) {
				/* If there is still valid data to send, load it from the receive buffer. */
				*destination ++ = UEDATX;
				/* Decrement the length. */
				length --;
			} else {
				/* Otherwise, flush the buffer. */
				break;
			}
		}

		/* Flush the receive buffer and reset the interrupt state machine. */
		UEINTX = (1 << NAKINI) | (1 << RWAL) | (1 << RXSTPI) | (1 << STALLEDI) | (1 << TXINI);
	}

	return lf_success;

failure:

	while (UEINTX & (1 << RWAL)) {
		/* Flush the receive buffer and reset the interrupt state machine. */
		UEINTX = (1 << NAKINI) | (1 << RWAL) | (1 << RXSTPI) | (1 << STALLEDI) | (1 << TXINI);
	}

	return lf_error;
}

/* Receive a packet using the appropriate bulk endpoint. */
int8_t megausb_bulk_transmit(uint8_t *source, lf_size_t length) {

	/* If USB is not configured, return with error. */
	if (!megausb_configured) {
		return lf_error;
	}

	/* Select the endpoint that has been configured to receive bulk data. */
	UENUM = BULK_IN_ENDPOINT & ~USB_IN_MASK;

	int total = lf_ceiling(length, BULK_OUT_SIZE);
	for (int i = 0; i < total; i ++) {

		/* Wait until the USB controller is ready. */
		volatile uint8_t timeout = UDFNUML + LF_USB_TIMEOUT_MS;
		while (!(UEINTX & (1 << RWAL))) {
			if (!megausb_configured || UDFNUML == timeout) {
				/* Reset the endpoint hardware. */
				UERST = 0x1E;
        		UERST = 0;
				goto failure;
			}
		}

		/* Transfer the buffered data to the destination. */
		uint8_t len = BULK_IN_SIZE;
		while (len --) {
			if (length) {
				/* If there is still valid data to send, load it into the transmit buffer. */
				UEDATX = *source ++;
				/* Decrement the length. */
				length --;
			} else {
				/* Otherwise, flush the buffer. */
				break;
			}
		}

		/* Flush the transmit buffer and reset the interrupt state machine. */
		UEINTX = (1 << RWAL) | (1 << NAKOUTI) | (1 << RXSTPI) | (1 << STALLEDI);
	}

	return lf_success;

failure:

	while (UEINTX & (1 << RWAL)) {
		/* Flush the transmit buffer and reset the interrupt state machine. */
		UEINTX = (1 << RWAL) | (1 << NAKOUTI) | (1 << RXSTPI) | (1 << STALLEDI);
	}

	return lf_error;
}
