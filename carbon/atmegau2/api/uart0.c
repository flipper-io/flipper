#include <flipper/uart0.h>

uint8_t idx = 0;

#define BAUDRATE(baud) (((F_CPU / (baud * 16.0)) + 0.5) - 1)

LF_FUNC("uart0") int uart0_configure(void) {

	UBRR1L = BAUDRATE(FMR_BAUD);
	UCSR1A &= ~(1 << U2X1);

	/* 8n1 */
	UCSR1C |= (1 << UCSZ11) | (1 << UCSZ10);

	/* Enable the receiver, transmitter, and receiver interrupt. */
	UCSR1B |= (1 << RXEN1) | (1 << TXEN1);
	UCSR1B |= (1 << RXCIE1);

	/* Enable the FSI line as an input. */
	FMR_DDR &= ~(1 << FMR_PIN);

	return lf_success;
}

LF_FUNC("uart0") int uart0_setbaud(uint32_t baud) {
	uint8_t b = BAUDRATE(baud);
	UBRR1L = b;
	return lf_success;
}

LF_FUNC("uart0") int uart0_reset(void) {
	idx = 0;
	while (UCSR1A & (1 << RXC1)) (void)UDR1;
	return lf_success;
}

LF_FUNC("uart0") int uart0_ready(void) {
	return (UCSR1A & (1 << RXC1)) || idx;
}

LF_FUNC("uart0") void uart0_enable(void) {
	UCSR1B = (1 << RXEN1) | (1 << TXEN1) | (1 << RXCIE1);
}

LF_FUNC("uart0") void uart0_disable(void) {
	UCSR1B &= ~((1 << RXEN1) | (1 << TXEN1) | (1 << RXCIE1));
}

LF_FUNC("uart0") void uart0_put(uint8_t byte) {
	uint8_t timeout = UDFNUML + LF_UART_TIMEOUT_MS;
	while (!(UCSR1A & (1 << UDRE1))) lf_assert(UDFNUML != timeout, fail, E_UART0_WRITE_TIMEOUT, "Error occurred while putting to uart0.");
	UDR1 = byte;
fail:
	return;
}

LF_FUNC("uart0") uint8_t uart0_get(void) {
	uint8_t b;
	uart0_read(&b, 1);
	return b;
}

LF_FUNC("uart0") int uart0_write(void *src, uint32_t length) {
	while (length --) uart0_put(*(uint8_t *)src++);
	return lf_success;
}

uint8_t uart0_buffer[64];

LF_FUNC("uart0") int uart0_read(void *dst, uint32_t length) {
	if (idx) {
		if (length >= idx) {
			memcpy(dst, uart0_buffer, idx);
			dst += idx;
			length -= idx;
			idx = 0;
		} else {
			memcpy(dst, uart0_buffer, length);
			memmove(uart0_buffer, uart0_buffer + length, idx - length);
			idx -= length;
			return lf_success;
		}
	}
	while (length--) {
		uint8_t timeout = UDFNUML + LF_UART_TIMEOUT_MS;
		while (!(UCSR1A & (1 << RXC1))) lf_assert(UDFNUML != timeout, fail, E_UART0_READ_TIMEOUT, "Timeout occurred while pulling from uart0.");
		*(uint8_t *) dst++ = UDR1;
	}
	return lf_success;

fail:
	return lf_error;
}

#include <flipper/atmegau2/megausb.h>

ISR(USART1_RX_vect) {
	while (!(UCSR1A & (1 << RXC1)));
	if (idx == sizeof(uart0_buffer)) idx = 0;
	uint8_t c = UDR1;
	if (FMR_IN & (1 << FMR_PIN)) {
		usb_debug_putchar(c);
	} else {
		uart0_buffer[idx++] = c;
	}
}
