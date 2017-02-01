#define __private_include__
#include <flipper/carbon/modules/uart0.h>
#include <flipper/carbon/platforms/atmega16u2.h>
#include <flipper/error.h>

/* Interrupt driven USART buffering. */
uint8_t usart_buffer[32];
uint8_t usart_index = 0;

int uart0_configure(void) {
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
	UCSR1B |= (1 << RXCIE1);
}

void uart0_disable(void) {
	UCSR1B &= ~((1 << RXEN1) | (1 << TXEN1));
	UCSR1B &= ~(1 << RXCIE1);
}

uint8_t uart0_ready(void) {
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

int uart0_push(void *source, lf_size_t length) {
	while (length --) uart0_put(*(uint8_t *)(source ++));
	return lf_success;
}

int uart0_pull(void *destination, lf_size_t length, uint32_t timeout) {
	while (length --) *(uint8_t *)(destination ++) = uart0_get(timeout);
	return lf_success;
}

ISR(USART1_RX_vect) {
	/* While there is data to read. */
	while (UCSR1A & (1 << RXC1)) {

	}
}
