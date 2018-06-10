#include <flipper/atmegau2/megausb.h>

int megausb_bulk_receive(void *dst, uint32_t length) {

    uint8_t _sreg = SREG;
	cli();

    lf_assert(megausb_configuration, E_USB, "usb is not configured");

	/* Select the endpoint that has been configured to receive bulk data. */
	UENUM = BULK_OUT_ENDPOINT;

	while (length) {

        /* TODO: use interrupts for USB timeout */

        /* Wait until the USB controller is ready. */
		uint8_t timeout = UDFNUML + LF_USB_TIMEOUT_MS;
		while (1) {

            /* data is available */
			if (UEINTX & (1 << RWAL)) break;

            /* TODO: do these need to be wrapped in SREG sets? */

			SREG = _sreg;
            lf_assert(UDFNUML != timeout, E_TIMEOUT, "usb transfer timed out");
            lf_assert(megausb_configuration, E_USB, "usb is not configured");
			_sreg = SREG;

			cli();
			UENUM = BULK_OUT_ENDPOINT;
		}

        /* calculate send length */
        size_t len = (length > BULK_OUT_SIZE) ? BULK_OUT_SIZE : length;
        /* fill the fifo with the available data */
        while (len --) *(uint8_t *)dst++ = UEDATX;
        /* fill the fifo */
        while ((UEINTX & (1 << RWAL))) (void)UEDATX;
        /* shift the fifo out */
        UEINTX = (1 << NAKINI) | (1 << RWAL) | (1 << RXSTPI) | (1 << STALLEDI) | (1 << TXINI);

        length -= len;
	}

	SREG = _sreg;

    /* TODO: figure out better way to handle SREG */

    SREG = _sreg;
    return lf_success;
fail:
    SREG = _sreg;
	return lf_error;
}

int megausb_bulk_transmit(void *src, uint32_t length) {

    uint8_t _sreg = SREG;
    cli();

    lf_assert(megausb_configuration, E_USB, "usb is not configured");

	/* Select the endpoint that has been configured to receive bulk data. */
	UENUM = BULK_IN_ENDPOINT & ~USB_IN_MASK;

	while (length) {

		/* Wait until the USB controller is ready. */
		uint8_t timeout = UDFNUML + LF_USB_TIMEOUT_MS;
		while (1) {

            /* data is available */
			if (UEINTX & (1 << RWAL)) break;

            /* TODO: do these need to be wrapped in SREG sets? */

			SREG = _sreg;
            lf_assert(UDFNUML != timeout, E_TIMEOUT, "usb transfer timed out");
            lf_assert(megausb_configuration, E_USB, "usb is not configured");
			_sreg = SREG;

			cli();
			UENUM = BULK_IN_ENDPOINT & ~USB_IN_MASK;
		}

        /* calculate send length */
        size_t len = (length > BULK_IN_SIZE) ? BULK_IN_SIZE : length;
        /* fill the fifo with the available data */
        while (len --) UEDATX = *(uint8_t *)src++;
        /* fill the fifo */
        while ((UEINTX & (1 << RWAL))) UEDATX = 0;
		/* shift the fifo out */
		UEINTX = (1 << RWAL) | (1 << NAKOUTI) | (1 << RXSTPI) | (1 << STALLEDI);

        length -= len;
	}

    /* TODO: figure out better way to handle SREG */

    SREG = _sreg;
    return lf_success;
fail:
    SREG = _sreg;
	return lf_error;
}
