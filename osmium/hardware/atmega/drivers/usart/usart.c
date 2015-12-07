#define __private_include__

#include <usart/usart.h>

#include <platform/atmega.h>

/* ~----------------------- USART0 -----------------------~ */

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

bool usart0_ready(void) {
	
	return (UCSR1A & (1 << RXC1));
	
}

void usart0_put(uint8_t byte) {
	
	while (!(UCSR1A & (1 << UDRE1)));
	
	UDR1 = byte;
	
}

uint8_t usart0_get(void) {
	
	//while (!(UCSR1A & (1 << RXC1)));
	
	return UDR1;
	
}

void usart0_push(void *source, uint32_t length) {
	
	while (length --) usart0_put(*(uint8_t *)(source ++));
	
}

void usart0_pull(void *destination, uint32_t length) {
	
	while (length --) *(uint8_t *)(destination ++) = usart0_get();
	
}

/* ~----------------------- USART1 -----------------------~ */

void usart1_configure(void *baud) {
	
	
	
}

void usart1_enable(void) {
	
	
	
}

void usart1_disable(void) {
	
	
	
}

bool usart1_ready(void) {
	
	return 0;
	
}

void usart1_put(uint8_t byte) {
	
	
	
}

uint8_t usart1_get(void) {
	
	return 0;
	
}

void usart1_push(void *source, uint32_t length) {
	
	
	
}

void usart1_pull(void *destination, uint32_t length) {
	
	
	
}

/* ~----------------------- DBGU -----------------------~ */

void dbgu_configure(void *baud) {
	
	
	
}

void dbgu_enable(void) {
	
	
	
}

void dbgu_disable(void) {
	
	
	
}

bool dbgu_ready(void) {
	
	return 0;
	
}

void dbgu_put(uint8_t byte) {
	
	
	
}

uint8_t dbgu_get(void) {
	
	return 0;
	
}

void dbgu_push(void *source, uint32_t length) {
	
	
	
}

void dbgu_pull(void *destination, uint32_t length) {
	
	
	
}