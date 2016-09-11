#define __private_include__
#include <flipper/uart.h>
#include <platform/atmega16u2.h>

/* ----------------------- USART0 ----------------------- */

void usart0_configure(void *baudrate) {
	uint16_t baud = (uint16_t)(baudrate);
	UBRR1H = hi(baud);
	UBRR1L = lo(baud);
	UCSR1C = (1 << USBS1) | (3 << UCSZ10);
	UCSR1B = (1 << RXEN1) | (1 << TXEN1);
	/* Enable the USART interrupt. */
	// UCSR1B |= (1 << RXCIE1);
}

void usart0_enable(void) {
	UCSR1B = (1 << RXEN1) | (1 << TXEN1);
}

void usart0_disable(void) {
	UCSR1B &= ~((1 << RXEN1) | (1 << TXEN1));
}

uint8_t usart0_ready(void) {
	return (UCSR1A & (1 << RXC1));
}

void usart0_put(uint8_t byte) {
	while (!(UCSR1A & (1 << UDRE1)));
	UDR1 = byte;
}


uint8_t usart0_get(void) {
	uint16_t timeout = 0;
	while (!(UCSR1A & (1 << RXC1)) && timeout != -1) timeout ++;
	return UDR1;
}

void usart0_push(void *source, uint32_t length) {
	while (length --) usart0_put(*(uint8_t *)(source ++));
}

void usart0_pull(void *destination, uint32_t length) {
	while (length --) *(uint8_t *)(destination ++) = usart0_get();
}
