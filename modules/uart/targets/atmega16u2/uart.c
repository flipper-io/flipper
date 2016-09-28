#define __private_include__
#include <flipper/uart.h>
#include <platform/atmega16u2.h>

void uart_configure(void) {
#define BAUD PLATFORM_BAUDRATE
#include <util/setbaud.h>
	UBRR1H = UBRRH_VALUE;
	UBRR1L = UBRRL_VALUE;
#if USE_2X
    UCSR1A |= (1 << U2X1);
#else
    UCSR1A &= ~(1 << U2X1);
#endif
	UCSR1C = (1 << USBS1) | (3 << UCSZ10);
	UCSR1B = (1 << RXEN1) | (1 << TXEN1);
	/* Enable the USART interrupt. */
	// UCSR1B |= (1 << RXCIE1);
}

void uart_enable(void) {
	UCSR1B = (1 << RXEN1) | (1 << TXEN1);
}

void uart_disable(void) {
	UCSR1B &= ~((1 << RXEN1) | (1 << TXEN1));
}

uint8_t uart_ready(void) {
	return (UCSR1A & (1 << RXC1));
}

void uart_put(uint8_t byte) {
	while (!(UCSR1A & (1 << UDRE1)));
	UDR1 = byte;
}


uint8_t uart_get(void) {
	uint16_t timeout = 0;
	while (!(UCSR1A & (1 << RXC1)) && timeout != -1) timeout ++;
	return UDR1;
}

#include <flipper/error.h>

void uart_push(void *source, uint32_t length) {
	while (length --) uart_put(*(uint8_t *)(source ++));
}

void uart_pull(void *destination, uint32_t length) {
	while (length --) *(uint8_t *)(destination ++) = uart_get();
}
