#define __private_include__
#include <flipper/uart0.h>
#include <platform/atmega16u2.h>
#include <flipper/error.h>

/* Interrupt driven USART buffering. */
uint8_t usart_buffer[32];
uint8_t usart_index = 0;

int uart0_configure(void) {
	/* 250k baud. */
	UBRR1H = 0x00;
	UBRR1L = 0x03;
	UCSR1A &= ~(1 << U2X1);
	/* 8n1 */
	UCSR1C = (1 << UCSZ10) | (1 << UCSZ11);
	/* Enable the receiver, transmitter, and receiver interrupt. */
	UCSR1B = (1 << RXEN1) | (1 << TXEN1);
	UCSR1B |= (1 << RXCIE1);
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

uint8_t uart0_get(void) {
	char byte = usart_buffer[0];
	usart_index = 0;
	return byte;
}

int uart0_push(void *source, lf_size_t length) {
	while (length --) uart0_put(*(uint8_t *)(source ++));
	return lf_success;
}

int uart0_pull(void *destination, lf_size_t length) {
	/* Ensure the length requested is not greater than the length of the USART buffer. */
	if (length > sizeof(usart_buffer)) {
		error_raise(E_OVERFLOW, NULL);
		return lf_error;
	}
	/* Wait until the incoming data has been buffered. */
	while ((UCSR1A & (1 << RXC1)));
	/* Write the UART buffer into the destination. */
	memcpy(destination, usart_buffer, length);
	/* Clear the buffer index. */
	usart_index = 0;
	return lf_success;
}

ISR(USART1_RX_vect) {
	/* Write the byte into the buffer. */
	while ((UCSR1A & (1 << RXC1))) {
		if (usart_index < sizeof(usart_buffer)) {
			usart_buffer[usart_index ++] = UDR1;
		} else {
			(void)UDR1;
		}
	}
}
