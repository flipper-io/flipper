#include "libflipper.h"
#include <atmegau2.h>
#include "megausb.h"

#define LF_USB_TIMEOUT_MS 3

#define lf_ceiling(x, y) ((x + y - 1) / y)

/* Receive a packet using the appropriate bulk endpoint. */
int megausb_bulk_receive(void *destination, uint32_t length) {

	/* If USB is not configured, return with error. */
	if (!megausb_configuration) {
		return lf_error;
	}

	uint8_t _sreg = SREG;
	cli();

	/* Select the endpoint that has been configured to receive bulk data. */
	UENUM = BULK_OUT_ENDPOINT;

	int total = lf_ceiling(length, BULK_OUT_SIZE);
	for (int i = 0; i < total; i ++) {

		/* Wait until the USB controller is ready. */
		uint8_t timeout = UDFNUML + LF_USB_TIMEOUT_MS;
		while (1) {

			if ((UEINTX & (1 << RWAL))) break;
			SREG = _sreg;

			if (UDFNUML == timeout) return lf_error;

			if (!megausb_configuration) return lf_error;

			_sreg = SREG;
			cli();

			UENUM = BULK_OUT_ENDPOINT;
		}

		/* Transfer the buffered data to the destination. */
		uint8_t len = BULK_OUT_SIZE;
		while (len --) {
			if (length) {
				/* If there is still valid data to send, load it from the receive buffer. */
				*(uint8_t *)destination++ = UEDATX;
				/* Decrement the length. */
				length --;
			}
		}

		/* Flush the receive buffer and reset the interrupt state machine. */
		UEINTX = (1 << NAKINI) | (1 << RWAL) | (1 << RXSTPI) | (1 << STALLEDI) | (1 << TXINI);
	}

	SREG = _sreg;
	return lf_success;

	// while (UEINTX & (1 << RWAL)) {
	// 	/* Flush the receive buffer and reset the interrupt state machine. */
	// 	UEINTX = (1 << NAKINI) | (1 << RWAL) | (1 << RXSTPI) | (1 << STALLEDI) | (1 << TXINI);
	// }
}

/* Receive a packet using the appropriate bulk endpoint. */
int megausb_bulk_transmit(void *source, uint32_t length) {

	/* If USB is not configured, return with error. */
	if (!megausb_configuration) {
		return lf_error;
	}

	uint8_t _sreg = SREG;
	cli();

	/* Select the endpoint that has been configured to receive bulk data. */
	UENUM = BULK_IN_ENDPOINT & ~USB_IN_MASK;

	int total = lf_ceiling(length, BULK_OUT_SIZE);
	for (int i = 0; i < total; i ++) {

		/* Wait until the USB controller is ready. */
		uint8_t timeout = UDFNUML + LF_USB_TIMEOUT_MS;
		while (1) {

			if ((UEINTX & (1 << RWAL))) break;
			SREG = _sreg;

			if (UDFNUML == timeout) return lf_error;

			if (!megausb_configuration) return lf_error;

			_sreg = SREG;
			cli();

			UENUM = BULK_IN_ENDPOINT & ~USB_IN_MASK;
		}

		/* Transfer the buffered data to the destination. */
		uint8_t len = BULK_IN_SIZE;
		while (len --) {
			if (length) {
				/* If there is still valid data to send, load it into the transmit buffer. */
				UEDATX = *(uint8_t *)source++;
				/* Decrement the length. */
				length --;
			}
		}

		/* Flush the transmit buffer and reset the interrupt state machine. */
		UEINTX = (1 << RWAL) | (1 << NAKOUTI) | (1 << RXSTPI) | (1 << STALLEDI);
	}

	SREG = _sreg;
	return lf_success;

	// while (UEINTX & (1 << RWAL)) {
	// 	/* Flush the transmit buffer and reset the interrupt state machine. */
	// 	UEINTX = (1 << RWAL) | (1 << NAKOUTI) | (1 << RXSTPI) | (1 << STALLEDI);
	// }

}
