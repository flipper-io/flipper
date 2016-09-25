#define __private_include__
#include <private/megausb.h>

static volatile uint8_t usb_configuration = 0;

void configure_usb(void) {
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
	usb_configuration = 0;
	/* Enable the end of reset and start of frame events to generate interrupts. */
	UDIEN = (1 << EORSTE) | (1 << SOFE);
	/* Enable interrupts. */
	sei();
}

/* Receive a packet using the appropriate interrupt endpoint. */
int8_t interrupt_receive_packet(uint8_t *destination) {

	/* If USB is not configured, return with error. */
	if (!usb_configuration) {
		return -1;
	}

	/* Calculate the timeout value using the frame counter. */
	uint8_t timeout = UDFNUML + DEFAULT_TIMEOUT;

	/* Select the endpoint that has been configured to receive interrupt data. */
	UENUM = INTERRUPT_RECEIVE_ENDPOINT;

	/* Wait until data has been received. */
	while (!(UEINTX & (1 << RWAL))) {
		/* If USB has been detached while in this loop, return with error. */
		if (!usb_configuration) {
			return -1;
		}
		/* If a timeout has occured, return 0 bytes sent. */
		else if (UDFNUML == timeout) {
			return 0;
		}
	}

	/* Transfer the buffered data to the destination. */
	uint8_t len = INTERRUPT_RECEIVE_SIZE;
	while (len --) *destination ++ = UEDATX;

	/* Re-enable interrupts for the receive endpoint. */
	UEINTX = (1 << NAKINI) | (1 << RWAL) | (1 << RXSTPI) | (1 << STALLEDI) | (1 << TXINI);

	return INTERRUPT_RECEIVE_SIZE;
}

/* Send a packet using the appropriate interrupt endpoint. */
int8_t interrupt_transmit_packet(uint8_t *source) {

	/* If USB is not configured, return with error. */
	if (!usb_configuration) {
		return -1;
	}

	/* Calculate the timeout value using the frame counter. */
	uint8_t timeout = UDFNUML + DEFAULT_TIMEOUT;

	/* Select the endpoint that has been configured to transmit interrupt data. */
	UENUM = INTERRUPT_TRANSMIT_ENDPOINT;

	/* Wait until data has been received. */
	while (!(UEINTX & (1 << RWAL))) {
		/* If USB has been detached while in this loop, return with error. */
		if (!usb_configuration) {
			return -1;
		}
		/* If a timeout has occured, return 0 bytes sent. */
		else if (UDFNUML == timeout) {
			return 0;
		}
	}

	/* Transfer the buffered data to the destination. */
	uint8_t len = INTERRUPT_TRANSMIT_SIZE;
	while (len --) UEDATX = *source ++;

	/* Re-enable interrupts for the transmit endpoint. */
	UEINTX = (1 << RWAL) | (1 << NAKOUTI) | (1 << RXSTPI) | (1 << STALLEDI);

	return INTERRUPT_TRANSMIT_SIZE;
}

/* Receive a packet using the appropriate bulk endpoint. */
int8_t bulk_receive_packet(uint8_t *destination) {

	/* If USB is not configured, return with error. */
	if (!usb_configuration) {
		return -1;
	}

	/* Calculate the timeout value using the frame counter. */
	uint8_t timeout = UDFNUML + DEFAULT_TIMEOUT;

	/* Select the endpoint that has been configured to receive bulk data. */
	UENUM = BULK_RECEIVE_ENDPOINT;

	/* Wait until data has been received. */
	while (!(UEINTX & (1 << RWAL))) {
		/* If USB has been detached while in this loop, return with error. */
		if (!usb_configuration) {
			return -1;
		}
		/* If a timeout has occured, return 0 bytes sent. */
		else if (UDFNUML == timeout) {
			return 0;
		}
	}

	/* Transfer the buffered data to the destination. */
	uint8_t len = BULK_RECEIVE_SIZE;
	while (len --) *destination ++ = UEDATX;

	/* Re-enable interrupts for the receive endpoint. */
	UEINTX = (1 << NAKINI) | (1 << RWAL) | (1 << RXSTPI) | (1 << STALLEDI) | (1 << TXINI);

	return BULK_RECEIVE_SIZE;
}

/* Receive a packet using the appropriate bulk endpoint. */
int8_t bulk_transmit_packet(uint8_t *source) {

	/* If USB is not configured, return with error. */
	if (!usb_configuration) {
		return -1;
	}

	/* Calculate the timeout value using the frame counter. */
	uint8_t timeout = UDFNUML + DEFAULT_TIMEOUT;

	/* Select the endpoint that has been configured to receive bulk data. */
	UENUM = BULK_TRANSMIT_ENDPOINT;

	/* Wait until data has been received. */
	while (!(UEINTX & (1 << RWAL))) {
		/* If USB has been detached while in this loop, return with error. */
		if (!usb_configuration) {
			return -1;
		}
		/* If a timeout has occured, return 0 bytes sent. */
		else if (UDFNUML == timeout) {
			return 0;
		}
	}

	/* Transfer the buffered data to the destination. */
	uint8_t len = BULK_TRANSMIT_SIZE;
	while (len --) UEDATX = *source ++;

	/* Re-enable interrupts for the receive endpoint. */
	UEINTX = (1 << RWAL) | (1 << NAKOUTI) | (1 << RXSTPI) | (1 << STALLEDI);

	return BULK_TRANSMIT_SIZE;
}

uint8_t transmit_previous_timeout = 0;

int8_t usb_serial_write(const uint8_t *buffer, uint16_t size)
{
	uint8_t timeout, intr_state, write_size;

	// if we're not online (enumerated and configured), error
	if (!usb_configuration) return -1;
	// interrupts are disabled so these functions can be
	// used from the main program or interrupt context,
	// even both in the same program!
	intr_state = SREG;
	cli();
	UENUM = BULK_TRANSMIT_ENDPOINT;
	// if we gave up due to timeout before, don't wait again
	if (transmit_previous_timeout) {
		if (!(UEINTX & (1<<RWAL))) {
			SREG = intr_state;
			return -1;
		}
		transmit_previous_timeout = 0;
	}
	// each iteration of this loop transmits a packet
	while (size) {
		// wait for the FIFO to be ready to accept data
		timeout = UDFNUML + DEFAULT_TIMEOUT;
		while (1) {
			// are we ready to transmit?
			if (UEINTX & (1<<RWAL)) break;
			SREG = intr_state;
			// have we waited too long?  This happens if the user
			// is not running an application that is listening
			if (UDFNUML == timeout) {
				transmit_previous_timeout = 1;
				return -1;
			}
			// has the USB gone offline?
			if (!usb_configuration) return -1;
			// get ready to try checking again
			intr_state = SREG;
			cli();
			UENUM = BULK_TRANSMIT_ENDPOINT;
		}

		// compute how many bytes will fit into the next packet
		write_size = BULK_TRANSMIT_SIZE - UEBCLX;
		if (write_size > size) write_size = size;
		size -= write_size;

		// write the packet
		switch (write_size) {
#if (BULK_TRANSMIT_SIZE == 64)
			case 64: UEDATX = *buffer++;
			case 63: UEDATX = *buffer++;
			case 62: UEDATX = *buffer++;
			case 61: UEDATX = *buffer++;
			case 60: UEDATX = *buffer++;
			case 59: UEDATX = *buffer++;
			case 58: UEDATX = *buffer++;
			case 57: UEDATX = *buffer++;
			case 56: UEDATX = *buffer++;
			case 55: UEDATX = *buffer++;
			case 54: UEDATX = *buffer++;
			case 53: UEDATX = *buffer++;
			case 52: UEDATX = *buffer++;
			case 51: UEDATX = *buffer++;
			case 50: UEDATX = *buffer++;
			case 49: UEDATX = *buffer++;
			case 48: UEDATX = *buffer++;
			case 47: UEDATX = *buffer++;
			case 46: UEDATX = *buffer++;
			case 45: UEDATX = *buffer++;
			case 44: UEDATX = *buffer++;
			case 43: UEDATX = *buffer++;
			case 42: UEDATX = *buffer++;
			case 41: UEDATX = *buffer++;
			case 40: UEDATX = *buffer++;
			case 39: UEDATX = *buffer++;
			case 38: UEDATX = *buffer++;
			case 37: UEDATX = *buffer++;
			case 36: UEDATX = *buffer++;
			case 35: UEDATX = *buffer++;
			case 34: UEDATX = *buffer++;
			case 33: UEDATX = *buffer++;
#endif
#if (BULK_TRANSMIT_SIZE >= 32)
			case 32: UEDATX = *buffer++;
			case 31: UEDATX = *buffer++;
			case 30: UEDATX = *buffer++;
			case 29: UEDATX = *buffer++;
			case 28: UEDATX = *buffer++;
			case 27: UEDATX = *buffer++;
			case 26: UEDATX = *buffer++;
			case 25: UEDATX = *buffer++;
			case 24: UEDATX = *buffer++;
			case 23: UEDATX = *buffer++;
			case 22: UEDATX = *buffer++;
			case 21: UEDATX = *buffer++;
			case 20: UEDATX = *buffer++;
			case 19: UEDATX = *buffer++;
			case 18: UEDATX = *buffer++;
			case 17: UEDATX = *buffer++;
#endif
#if (BULK_TRANSMIT_SIZE >= 16)
			case 16: UEDATX = *buffer++;
			case 15: UEDATX = *buffer++;
			case 14: UEDATX = *buffer++;
			case 13: UEDATX = *buffer++;
			case 12: UEDATX = *buffer++;
			case 11: UEDATX = *buffer++;
			case 10: UEDATX = *buffer++;
			case  9: UEDATX = *buffer++;
#endif
			case  8: UEDATX = *buffer++;
			case  7: UEDATX = *buffer++;
			case  6: UEDATX = *buffer++;
			case  5: UEDATX = *buffer++;
			case  4: UEDATX = *buffer++;
			case  3: UEDATX = *buffer++;
			case  2: UEDATX = *buffer++;
			default:
			case  1: UEDATX = *buffer++;
			case  0: break;
		}
		// if this completed a packet, transmit it now!
		if (!(UEINTX & (1<<RWAL))) UEINTX = 0x3A;
		SREG = intr_state;
	}
	return 0;
}

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
		usb_configuration = 0;
	}

	/* Evaluates when a start of frame interrupt is received. Occurs once every millisecond. */
	if ((_udint & (1 << SOFI)) && usb_configuration) {

	}

}

static inline void usb_wait_in_ready(void) {
	while (!(UEINTX & (1 << TXINI))) ;
}

static inline void usb_send_in(void) {
	UEINTX = ~(1 << TXINI);
}

static inline void usb_wait_receive_out(void) {
	while (!(UEINTX & (1 << RXOUTI))) ;
}

static inline void usb_ack_out(void) {
	UEINTX = ~(1 << RXOUTI);
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
			usb_configuration = wValue;
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
			UEDATX = usb_configuration;
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
