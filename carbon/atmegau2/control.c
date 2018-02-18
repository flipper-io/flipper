#include <flipper/atmegau2/megausb.h>

volatile uint8_t megausb_configuration = 0;

int usb_configure(void) {
	/* Enable the USB hardware for configuration, but freeze the clock. */
	USBCON = (1 << USBE) | (1 << FRZCLK);
	/* Configure the USB PLL to use the 16MHz system clock. */
	PLLCSR = (1 << PLLE) | (1 << PLLP0);
	/* Wait until USB PLL configuration has succeeded. */
	while (!(PLLCSR & (1 << PLOCK)));
	/* Unfreeze the USB clock and enable the USB hardware. */
	USBCON = (1 << USBE);
	/* Attach the USB device. */
	UDCON = 0;
	/* Clear the current USB configuration. */
	megausb_configuration = 0;
	/* Enable the end of reset and start of frame events to generate interrupts. */
	UDIEN = (1 << EORSTE) | (1 << SOFE);
	return lf_success;
}

extern volatile uint8_t debug_flush_timer;

/* This is the 'general' USB interrupt service routine. It handles all non CONTROL related interrupts. */
ISR(USB_GEN_vect) {

	uint8_t _udint = UDINT;

	/* Clear all USB interrupts. */
	UDINT = 0;

	/* Evaluates when an end of reset interrupt is received. */
	if (_udint & (1 << EORSTI)) {
		/* Select endpoint 0 for configuration. */
		UENUM = USB_CONTROL_ENDPOINT;
		/* Enable endpoint 0 by setting the EPEN bit. */
		UECONX = (1 << EPEN);
		/* Configure endpoint 0 as the control endpoint. */
		UECFG0X = ENDPOINT_TYPE_CONTROL;
		/* Set the size of the control endpoint, and use single buffering. */
		UECFG1X = ENDPOINT_SIZE(USB_CONTROL_SIZE) | ENDPOINT_SINGLE_BUFFER;
		/* Let setup packets generate interrupts. */
		UEIENX = (1 << RXSTPE);
		/* Zero the USB configuration. */
		megausb_configuration = 0;
	}

	uint8_t t;

	/* Evaluates when a start of frame interrupt is received. Occurs once every millisecond. */
	if ((_udint & (1 << SOFI)) && megausb_configuration) {

		t = debug_flush_timer;
		if (t) {
			debug_flush_timer = --t;
			if (!t) {
				UENUM = DEBUG_IN_ENDPOINT;
				while ((UEINTX & (1 << RWAL))) UEDATX = '\0';
				UEINTX = 0x3A;
			}
		}

	}

}

static inline void usb_wait_in_ready(void) {
	while (!(UEINTX & (1 << TXINI))) ;
}

static inline void usb_send_in(void) {
	UEINTX = ~(1 << TXINI);
}

/* This is the 'communications' USB interrupt service routine. It is dedicated to handling CONTROL requests. */
ISR(USB_COM_vect) {
	uint8_t intbits;
	const uint8_t *list;
	const uint8_t *cfg;
	uint8_t i, n, len, en;
	uint8_t bmRequestType;
	uint8_t bRequest;
	uint16_t wValue;
	uint16_t wIndex;
	uint16_t wLength;
	uint16_t desc_val;
	const uint8_t *desc_addr;
	uint8_t	desc_length;

	/* Select the control endpoint. */
	UENUM = USB_CONTROL_ENDPOINT;

	intbits = UEINTX;
	if (intbits & (1 << RXSTPI)) {

		bmRequestType = UEDATX;
		bRequest = UEDATX;
		wValue = UEDATX;
		wValue |= (UEDATX << 8);
		wIndex = UEDATX;
		wIndex |= (UEDATX << 8);
		wLength = UEDATX;
		wLength |= (UEDATX << 8);

		UEINTX = ~((1 << RXSTPI) | (1 << RXOUTI) | (1 << TXINI));

		if (bRequest == GET_DESCRIPTOR) {
			list = (const uint8_t *)descriptors;
			for (i=0; ; i++) {
				if (i >= NUM_DESC_LIST) {
					UECONX = (1 << STALLRQ) | (1 << EPEN);  //stall
					return;
				}
				desc_val = pgm_read_word(list);
				if (desc_val != wValue) {
					list += sizeof(struct descriptor);
					continue;
				}
				list += 2;
				desc_val = pgm_read_word(list);
				if (desc_val != wIndex) {
					list += sizeof(struct descriptor)-2;
					continue;
				}
				list += 2;
				desc_addr = (const uint8_t *)pgm_read_word(list);
				list += 2;
				desc_length = pgm_read_byte(list);
				break;
			}
			len = (wLength < 256) ? wLength : 255;
			if (len > desc_length) len = desc_length;
			do {
				do {
					i = UEINTX;
				} while (!(i & ((1 << TXINI) | (1 << RXOUTI))));
				if (i & (1 << RXOUTI)) return;
				n = len < USB_CONTROL_SIZE ? len : USB_CONTROL_SIZE;
				for (i = n; i; i--) {
					UEDATX = pgm_read_byte(desc_addr++);
				}
				len -= n;
				usb_send_in();
			} while (len || n == USB_CONTROL_SIZE);
			return;
		}
		if (bRequest == SET_ADDRESS) {
			usb_send_in();
			usb_wait_in_ready();
			UDADDR = wValue | (1 << ADDEN);
			return;
		}
		if (bRequest == SET_CONFIGURATION && bmRequestType == 0) {
			megausb_configuration = wValue;
			usb_send_in();
			cfg = endpoint;
			for (i = 1; i < 5; i++) {
				UENUM = i;
				en = pgm_read_byte(cfg++);
				UECONX = en;
				if (en) {
					UECFG0X = pgm_read_byte(cfg++);
					UECFG1X = pgm_read_byte(cfg++);
				}
			}
			UERST = 0x1E;
			UERST = 0;
			return;
		}
		if (bRequest == GET_CONFIGURATION && bmRequestType == 0x80) {
			usb_wait_in_ready();
			UEDATX = megausb_configuration;
			usb_send_in();
			return;
		}
		if (bRequest == GET_STATUS) {
			usb_wait_in_ready();
			i = 0;
			if (bmRequestType == 0x82) {
				UENUM = wIndex;
				if (UECONX & (1 << STALLRQ)) i = 1;
				UENUM = 0;
			}
			UEDATX = i;
			UEDATX = 0;
			usb_send_in();
			return;
		}
		if ((bRequest == CLEAR_FEATURE || bRequest == SET_FEATURE)
			&& bmRequestType == 0x02 && wValue == 0) {
			i = wIndex & 0x7F;
			if (i >= 1 && i <= MAX_ENDPOINT) {
				usb_send_in();
				UENUM = i;
				if (bRequest == SET_FEATURE)
					UECONX = (1 << STALLRQ) | (1 << EPEN);
				else {
					UECONX = (1 << STALLRQC) | (1 << RSTDT) | (1 << EPEN);
					UERST = (1 << i);
					UERST = 0;
				}
				return;
			}
		}
	}
	UECONX = (1 << STALLRQ) | (1 << EPEN);
}
