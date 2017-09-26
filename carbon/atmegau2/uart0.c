#define __private_include__
#include <flipper/uart0.h>
#include <flipper/atmegau2/atmegau2.h>
#include <flipper/error.h>

/* Interrupt driven USART buffering. */
uint8_t usart_buffer[32];
uint8_t usart_index = 0;

int uart0_configure(struct _lf_endpoint *endpoint, void *_ctx) {
	/* 2  megabaud. */
	UBRR1H = 0x00;
	UBRR1L = 0x00;
	UCSR1A = (1 << U2X1);
	/* 8n1 */
	UCSR1C = (1 << UCSZ10) | (1 << UCSZ11);
	/* Enable the receiver, transmitter, and receiver interrupt. */
	UCSR1B = (1 << RXEN1) | (1 << TXEN1);
//	UCSR1B |= (1 << RXCIE1);
	return lf_success;
}

void uart0_enable(void) {
	UCSR1B = (1 << RXEN1) | (1 << TXEN1);
//	UCSR1B |= (1 << RXCIE1);
}

void uart0_disable(void) {
	UCSR1B &= ~((1 << RXEN1) | (1 << TXEN1));
	UCSR1B &= ~(1 << RXCIE1);
}

bool uart0_ready(struct _lf_endpoint *endpoint) {
	return (UCSR1A & (1 << RXC1));
}

void uart0_put(uint8_t byte) {
	while (!(UCSR1A & (1 << UDRE1)));
	UDR1 = byte;
}

uint8_t uart0_get(volatile uint32_t timeout) {
	while (!(UCSR1A & (1 << RXC1)) && timeout--);
	return UDR1;
}

int uart0_push(struct _lf_endpoint *endpoint, void *source, lf_size_t length) {
	while (length --) uart0_put(*(uint8_t *)(source ++));
	return lf_success;
}

int uart0_pull(struct _lf_endpoint *endpoint, void *destination, lf_size_t length) {
	while (length --) *(uint8_t *)(destination ++) = uart0_get(0xFF);
	return lf_success;
}

#pragma warning Move this.

void uart0_dfu(void) {
	/* 115.2k baud for DFU communication. */
	UBRR1L = 0x08;
	/* Don't multiply baud by 2. */
	UCSR1A &= ~(1 << U2X1);
}

ISR(USART1_RX_vect) {
	/* While there is data to read. */
	while (UCSR1A & (1 << RXC1)) {

	}
}
