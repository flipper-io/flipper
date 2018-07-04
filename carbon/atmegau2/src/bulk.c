#include "libflipper.h"
#include "atmegau2.h"
#include "megausb.h"

#define LF_USB_TIMEOUT_S 2

int megausb_bulk_receive(void *dst, uint32_t length) {

	lf_assert(megausb_configuration, E_USB, "usb is not configured");

	/* Select the endpoint that has been configured to receive bulk data. */
	UENUM = BULK_OUT_ENDPOINT;

	while (length) {

		/* TODO: use interrupts for USB timeout */

		while (!(UEINTX & (1 << RWAL))) __asm__ __volatile__("nop");

		/* calculate send length */
		size_t len = (length > BULK_OUT_SIZE) ? BULK_OUT_SIZE : length;
		/* fill the fifo with the available data */
		while (len --) *(uint8_t *)dst++ = UEDATX;
		/* clear the fifo */
		UEINTX = (1 << NAKINI) | (1 << RWAL) | (1 << RXSTPI) | (1 << STALLEDI) | (1 << TXINI);

		length -= len;
	}

	/* TODO: figure out better way to handle SREG */

	return lf_success;
fail:
	return lf_error;
}

int megausb_bulk_transmit(void *src, uint32_t length) {

	uint8_t _sreg = SREG;
	cli();

	lf_assert(megausb_configuration, E_USB, "usb is not configured");

	/* Select the endpoint that has been configured to receive bulk data. */
	UENUM = BULK_IN_ENDPOINT & ~USB_IN_MASK;

	lf_debug("sending %i from bulk", length);

	while (length) {

		/* Wait until the USB controller is ready. */
		uint8_t timeout = UDFNUMH + LF_USB_TIMEOUT_S;

		while (1) {
			SREG = _sreg;

			/* data is available */
			if (UEINTX & (1 << RWAL)) break;

			/* TODO: do these need to be wrapped in SREG sets? */
			lf_assert(UDFNUMH != timeout, E_TIMEOUT, "usb transfer timed out");
			lf_assert(megausb_configuration, E_USB, "usb is not configured");

			_sreg = SREG;
			cli();
			UENUM = BULK_IN_ENDPOINT & ~USB_IN_MASK;

		}

		/* calculate send length */
		size_t len = (length > BULK_IN_SIZE) ? BULK_IN_SIZE : length;
		/* fill the fifo with the available data */
		while (len --) UEDATX = *(uint8_t *)src++;
		/* shift the fifo out */
		UEINTX = (1 << RWAL) | (1 << NAKOUTI) | (1 << RXSTPI) | (1 << STALLEDI);

		length -= len;
	}

  /* TODO: figure out better way to handle SREG */

//	SREG = _sreg;
	return lf_success;
fail:
//	SREG = _sreg;
	return lf_error;
}
