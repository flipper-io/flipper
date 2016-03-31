#define __private_include__
#include <flipper/usart.h>
#include <flipper/platform/platform.h>

/* ~ ----------------------- USART0 ----------------------- ~ */

void usart0_configure(void *baud) {

	UBRR1H = hi((uint16_t)(baud));
	UBRR1L = lo((uint16_t)(baud));

	UCSR1C = (1 << USBS1) | (3 << UCSZ10);
	UCSR1B = (1 << RXEN1) | (1 << TXEN1);

	/* ~ Enable the USART interrupt. ~ */

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

uint16_t usart_timeout = 0;

uint8_t usart0_get(void) {

	while (!(UCSR1A & (1 << RXC1)) && usart_timeout != -1) usart_timeout ++;

	usart_timeout = 0;

	return UDR1;

}

void usart0_push(void *source, uint32_t length) {

	while (length --) usart0_put(*(uint8_t *)(source ++));

}

void usart0_pull(void *destination, uint32_t length) {

	while (length --) *(uint8_t *)(destination ++) = usart0_get();

}
