#define __private_include__
#include <flipper/uart0.h>
#include <flipper/atmegau2/atmegau2.h>
#include <flipper/error.h>

int uart0_configure(void *_ctx) {

	/* 115.2k baud for DFU communication. */
	UBRR1L = 0x10;
	UCSR1A |= (1 << U2X1);

	// if (_ctx == 0) {
	// 	/* 2  megabaud. */
	// 	UBRR1H = 0x00;
	// 	UBRR1L = 0x00;
	// } else {
	// 	/* 115.2k baud for DFU communication. */
	// 	UBRR1L = 0x08;
	// 	/* Don't multiply baud by 2. */
	// 	UCSR1A &= ~(1 << U2X1);
	// }

	/* 8n1 */
	UCSR1C = (1 << UCSZ10) | (1 << UCSZ11);
	/* Enable the receiver, transmitter, and receiver interrupt. */
	UCSR1B = (1 << RXEN1) | (1 << TXEN1);
//	UCSR1B |= (1 << RXCIE1);
	return lf_success;
}

int uart0_ready(void) {
	return (UCSR1A & (1 << RXC1));
}

int uart0_push(void *source, lf_size_t length) {
	while (length --) {
		while (!(UCSR1A & (1 << UDRE1)));
		UDR1 = *(uint8_t *)(source ++);
	}
	return lf_success;
}

int uart0_pull(void *destination, lf_size_t length) {
	while (length --) {
		uint8_t timeout = UDFNUML + LF_UART_TIMEOUT_MS;
		while (!(UCSR1A & (1 << RXC1))) {
			if (UDFNUML == timeout) return -1;
		}
		*(uint8_t *)(destination ++) = UDR1;
	}
	return lf_success;
}
