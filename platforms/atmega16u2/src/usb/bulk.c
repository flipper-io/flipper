#define __private_include__
#include <private/megausb.h>

/* Receive a packet using the appropriate bulk endpoint. */
int8_t bulk_receive_packet(uint8_t *destination) {

	/* If USB is not configured, return with error. */
	if (!megausb_configured) {
		return -1;
	}

	/* Calculate the timeout value using the frame counter. */
	uint8_t timeout = UDFNUML + DEFAULT_TIMEOUT;

	/* Select the endpoint that has been configured to receive bulk data. */
	UENUM = BULK_OUT_ENDPOINT;

	/* Wait until data has been received. */
	while (!(UEINTX & (1 << RWAL))) {
		/* If USB has been detached while in this loop, return with error. */
		if (!megausb_configured) {
			return -1;
		}
		/* If a timeout has occured, return 0 bytes sent. */
		else if (UDFNUML == timeout) {
			return 0;
		}
	}

	/* Transfer the buffered data to the destination. */
	uint8_t len = BULK_OUT_SIZE;
	while (len --) *destination ++ = UEDATX;

	/* Re-enable interrupts for the receive endpoint. */
	UEINTX = (1 << NAKINI) | (1 << RWAL) | (1 << RXSTPI) | (1 << STALLEDI) | (1 << TXINI);

	return BULK_OUT_SIZE;
}

/* Receive a packet using the appropriate bulk endpoint. */
int8_t bulk_transmit_packet(uint8_t *source) {

	/* If USB is not configured, return with error. */
	if (!megausb_configured) {
		return -1;
	}

	/* Calculate the timeout value using the frame counter. */
	uint8_t timeout = UDFNUML + DEFAULT_TIMEOUT;

	/* Select the endpoint that has been configured to receive bulk data. */
	UENUM = BULK_IN_ENDPOINT & ~USB_IN_MASK;

	/* Wait until data has been received. */
	while (!(UEINTX & (1 << RWAL))) {
		/* If USB has been detached while in this loop, return with error. */
		if (!megausb_configured) {
			return -1;
		}
		/* If a timeout has occured, return 0 bytes sent. */
		else if (UDFNUML == timeout) {
			return 0;
		}
	}

	/* Transfer the buffered data to the destination. */
	uint8_t len = BULK_IN_SIZE;
	while (len --) UEDATX = *source ++;

	/* Re-enable interrupts for the receive endpoint. */
	UEINTX = (1 << RWAL) | (1 << NAKOUTI) | (1 << RXSTPI) | (1 << STALLEDI);

	return BULK_IN_SIZE;
}
