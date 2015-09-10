#define __private_include__

#include <platform/hid.h>

static volatile uint8_t usb_configuration=0;

static volatile uint8_t rx_timeout_count=0;

static volatile uint8_t tx_timeout_count=0;

void configure_usb(void) {
	
	usb_freeze();
	
	usb_configure_pll();
	
	while (!(PLLCSR & (1 << PLOCK)));
	
	usb_configure_hardware();
	
	UDCON = 0;
	
	usb_configuration = 0;
	
	UDIEN = (1 << EORSTE) | (1 << SOFE);
	
	sei();
	
	while (!usb_configuration);
	
}

int8_t usb_receive_packet(uint8_t *buffer) {
	
	uint8_t intr_state;
	
	if (!usb_configuration) return -1;
	
	intr_state = SREG;
	
	cli();
	
	rx_timeout_count = DEFAULT_TIMEOUT;
	
	UENUM = RAWHID_RX_ENDPOINT;
	
	while (1) {
		
		if (UEINTX & (1 << RWAL)) break;
		
		SREG = intr_state;
		
		if (rx_timeout_count == 0) return 0;
		
		if (!usb_configuration) return -1;
		
		intr_state = SREG;
		
		cli();
		
		UENUM = RAWHID_RX_ENDPOINT;
		
	}
	
#if (HID_RECEIVE_SIZE >= 64)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 63)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 62)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 61)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 60)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 59)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 58)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 57)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 56)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 55)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 54)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 53)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 52)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 51)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 50)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 49)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 48)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 47)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 46)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 45)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 44)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 43)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 42)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 41)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 40)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 39)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 38)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 37)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 36)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 35)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 34)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 33)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 32)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 31)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 30)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 29)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 28)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 27)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 26)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 25)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 24)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 23)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 22)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 21)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 20)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 19)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 18)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 17)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 16)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 15)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 14)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 13)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 12)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 11)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 10)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 9)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 8)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 7)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 6)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 5)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 4)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 3)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 2)
	
	*buffer ++ = UEDATX;
	
#endif
	
#if (HID_RECEIVE_SIZE >= 1)
	
	*buffer ++ = UEDATX;
	
#endif
	
	UEINTX = 0x6B;
	
	SREG = intr_state;
	
	return HID_RECEIVE_SIZE;
	
}

int8_t usb_send(void) {
	
	
	
	return 0;
	
}

int8_t usb_send_packet(uint8_t *buffer) {
	
	uint8_t intr_state;
	
	if (!usb_configuration) return -1;
	
	intr_state = SREG;
	
	cli();
	
	tx_timeout_count = DEFAULT_TIMEOUT;
	
	UENUM = RAWHID_TX_ENDPOINT;
	
	while (1) {
		
		if (UEINTX & (1 << RWAL)) break;
		
		SREG = intr_state;
		
		if (tx_timeout_count == 0) return 0;
		
		if (!usb_configuration) return -1;
		
		intr_state = SREG;
		
		cli();
		
		UENUM = RAWHID_TX_ENDPOINT;
		
	}
	
#if (HID_TRANSMIT_SIZE >= 64)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 63)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 62)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 61)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 60)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 59)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 58)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 57)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 56)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 55)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 54)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 53)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 52)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 51)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 50)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 49)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 48)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 47)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 46)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 45)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 44)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 43)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 42)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 41)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 40)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 39)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 38)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 37)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 36)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 35)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 34)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 33)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 32)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 31)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 30)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 29)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 28)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 27)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 26)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 25)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 24)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 23)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 22)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 21)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 20)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 19)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 18)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 17)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 16)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 15)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 14)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 13)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 12)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 11)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 10)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 9)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 8)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 7)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 6)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 5)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 4)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 3)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 2)
	
	UEDATX = *buffer ++;
	
#endif
	
#if (HID_TRANSMIT_SIZE >= 1)
	
	UEDATX = *buffer ++;
	
#endif
	
	UEINTX = 0x3A;
	
	SREG = intr_state;
	
	return HID_TRANSMIT_SIZE;
	
}

bool something = false;

int previntbits = 0;

static volatile uint8_t debug_flush_timer=0;

ISR(USB_GEN_vect) {
	
	uint8_t intbits, t;
	
	intbits = UDINT;
	
	previntbits = intbits;
	
	UDINT = 0;
	
	if (intbits & (1 << EORSTI)) {
		
		UENUM = 0;
		
		UECONX = 1;
		
		UECFG0X = EP_TYPE_CONTROL;
		
		UECFG1X = EP_SIZE(ENDPOINT0_SIZE) | EP_SINGLE_BUFFER;
		
		UEIENX = (1 << RXSTPE);
		
		usb_configuration = 0;
		
	}
	
	if ((intbits & (1 << SOFI)) && usb_configuration) {
		
		t = debug_flush_timer;
		if (t) {
			debug_flush_timer = -- t;
			if (!t) {
				UENUM = DEBUG_TX_ENDPOINT;
				while ((UEINTX & (1<<RWAL))) {
					UEDATX = 0;
				}
				UEINTX = 0x3A;
				
			}
		}
		
		t = rx_timeout_count;
		
		if (t) rx_timeout_count = --t;
		
		t = tx_timeout_count;
		
		if (t) tx_timeout_count = --t;
		
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
	
	UENUM = 0;
	
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
				
				n = len < ENDPOINT0_SIZE ? len : ENDPOINT0_SIZE;
				
				for (i = n; i; i--) {
					
					UEDATX = pgm_read_byte(desc_addr++);
					
				}
				
				len -= n;
				
				usb_send_in();
				
			} while (len || n == ENDPOINT0_SIZE);
			
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
		
		if (wIndex == HID_INTERFACE) {
			
			if (bmRequestType == 0xA1 && bRequest == HID_GET_REPORT) {
				
				len = HID_TRANSMIT_SIZE;
				
				do {
					
					do {
						
						i = UEINTX;
						
					} while (!(i & ((1 << TXINI) | (1 << RXOUTI))));
					
					if (i & (1 << RXOUTI)) return;
					
					n = len < ENDPOINT0_SIZE ? len : ENDPOINT0_SIZE;
					
					for (i = n; i; i --)
						
						UEDATX = 0;
					
					len -= n;
					
					usb_send_in();
					
				} while (len || n == ENDPOINT0_SIZE);
				
				return;
				
			}
			
			if (bmRequestType == 0x21 && bRequest == HID_SET_REPORT) {
				
				len = HID_RECEIVE_SIZE;
				
				do {
					
					n = len < ENDPOINT0_SIZE ? len : ENDPOINT0_SIZE;
					
					usb_wait_receive_out();
					
					usb_ack_out();
					
					len -= n;
					
				} while (len);
				
				usb_wait_in_ready();
				
				usb_send_in();
				
				return;
				
			}
			
		}
		
	}
	
	UECONX = (1 << STALLRQ) | (1 << EPEN);
	
}

int8_t debug_send_byte(uint8_t c) {
	
	static uint8_t previous_timeout = 0;
	
	uint8_t timeout, intr_state;
	
	if (!usb_configuration) return -1;
	
	intr_state = SREG;
	
	cli();
	
	UENUM = DEBUG_TX_ENDPOINT;
	
	if (previous_timeout) {
		
		if (!(UEINTX & (1<<RWAL))) {
			
			SREG = intr_state;
			
			return -1;
			
		}
		
		previous_timeout = 0;
		
	}
	
	timeout = UDFNUML + 3;
	
	while (1) {
		
		if (UEINTX & (1<<RWAL)) break;
		
		SREG = intr_state;
		
		if (UDFNUML == timeout) {
			
			previous_timeout = 1;
			
			return -1;
			
		}
		
		if (!usb_configuration) return -1;
		
		intr_state = SREG;
		
		cli();
		
		UENUM = DEBUG_TX_ENDPOINT;
		
	}
	
	UEDATX = c;
	
	if (!(UEINTX & (1<<RWAL))) {
		
		UEINTX = 0x3A;
		
		debug_flush_timer = 0;
		
	}
	
	else {
		
		debug_flush_timer = 2;
		
	}
	
	SREG = intr_state;
	
	return 0;
}