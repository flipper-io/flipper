#include <flipper/atmegau2/megausb.h>

/* Receive a packet using the appropriate interrupt endpoint. */
int8_t megausb_interrupt_receive(void *dst, uint32_t length) {

	/* If USB is not configured, return with error. */
	if (!megausb_configuration) {
		return lf_error;
	}

	/* Select the endpoint that has been configured to receive interrupt data. */
	UENUM = INTERRUPT_OUT_ENDPOINT;

	int total = lf_ceiling(length, INTERRUPT_OUT_SIZE);
	for (int i = 0; i < total; i ++) {

		/* Wait until the USB controller is ready. */
		volatile uint8_t timeout = UDFNUML + LF_USB_TIMEOUT_MS;
		while (!(UEINTX & (1 << RWAL)) && megausb_configuration) {
#ifdef __lf_usb_timeout__
			if (UDFNUML == timeout) {
				/* Reset the endpoint hardware. */
				UERST = 0x1E;
				UERST = 0;
				goto fail;
			}
#endif
		}

		/* Transfer the buffered data to the dst. */
		uint8_t len = INTERRUPT_OUT_SIZE;
		while (len --) {
			if (length --) {
				/* If there is still valid data to send, load it from the receive buffer. */
				*(uint8_t *)dst++ = UEDATX;
			} else {
				/* Otherwise, flush the buffer. */
				break;
			}
		}

		/* Re-enable interrupts for the receive endpoint. */
		UEINTX = (1 << NAKINI) | (1 << RWAL) | (1 << RXSTPI) | (1 << STALLEDI) | (1 << TXINI);
	}

	return lf_success;

fail:

	return lf_error;
}

/* Send a packet using the appropriate interrupt endpoint. */
int8_t megausb_interrupt_transmit(void *src, uint32_t length) {

	/* If USB is not configured, return with error. */
	if (!megausb_configuration) {
		return lf_error;
	}

	/* Select the endpoint that has been configured to transmit interrupt data. */
	UENUM = INTERRUPT_IN_ENDPOINT & ~USB_IN_MASK;

	int total = lf_ceiling(length, INTERRUPT_IN_SIZE);
	for (int i = 0; i < total; i ++) {

		/* Wait until the USB controller is ready. */
		volatile uint8_t timeout = UDFNUML + LF_USB_TIMEOUT_MS;
		while (!(UEINTX & (1 << RWAL)) && megausb_configuration) {
#ifdef __lf_usb_timeout__
			if (UDFNUML == timeout) {
				/* Reset the endpoint hardware. */
				UERST = 0x1E;
				UERST = 0;
				goto fail;
			}
#endif
		}

		/* Transfer the buffered data to the dst. */
		uint8_t len = INTERRUPT_IN_SIZE;
		while (len --) {
			if (length --) {
				UEDATX = *(uint8_t *)src++;
			} else {
				/* Otherwise, flush the buffer. */
				break;
			}
		}

		/* Re-enable interrupts for the transmit endpoint. */
		UEINTX = (1 << RWAL) | (1 << NAKOUTI) | (1 << RXSTPI) | (1 << STALLEDI);
	}

	return lf_success;

fail:

	return lf_error;
}
