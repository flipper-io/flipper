#define __private_include__

#include <drivers/usart.h>

#include <platform/atmega.h>

/* ------------------------ USART0 ------------------------ */

void usart0_configure(uint16_t baud) {
	
	UBRR1H = hi(baud);
	
	UBRR1L = lo(baud);
	
	UCSR1B = (1 << RXEN1) | (1 << TXEN1);
	
	UCSR1C = (1 << USBS1) | (3 << UCSZ10);
	
	/* ~ Enable the USART interrupt. ~ */
	
	UCSR1B |= (1 << RXCIE1);
	
}

void usart0_enable(void) {
	
	
	
}

void usart0_disable(void) {
	
	
	
}

bool usart0_ready(void) {
	
	return (UCSR1A & (1 << RXC1));
	
}

void usart0_put_byte(uint8_t byte) {
	
	while (!(UCSR1A & (1 << UDRE1)));
	
	UDR1 = byte;
	
}

uint8_t usart0_get_byte(void) {
	
	while (!(UCSR1A & (1 << RXC1)));
	
	return UDR1;
	
}

void usart0_push(void *source, uint32_t length) {
	
	while (length --) usart0_put_byte(*(uint8_t *)(source ++));
	
}

void usart0_pull(void *destination, uint32_t length) {
	
	while (length --) *(uint8_t *)(destination ++) = usart0_get_byte();
	
}

/* ------------------------ USART1 ------------------------ */

void usart1_configure(uint16_t baud) {
	
	
	
}

void usart1_enable(void) {
	
	
	
}

void usart1_disable(void) {
	
	
	
}

bool usart1_ready(void) {
	
	return 0;
	
}

void usart1_put_byte(uint8_t byte) {
	
	
	
}

uint8_t usart1_get_byte(void) {
	
	return 0;
	
}

void usart1_push(void *source, uint32_t length) {
	
	
	
}

void usart1_pull(void *destination, uint32_t length) {
	
	
	
}

/* ------------------------ DBGU ------------------------ */

void dbgu_configure(uint16_t baud) {
	
	
	
}

void dbgu_enable(void) {
	
	
	
}

void dbgu_disable(void) {
	
	
	
}

bool dbgu_ready(void) {
	
	return 0;
	
}

void dbgu_put_byte(uint8_t byte) {
	
	
	
}

uint8_t dbgu_get_byte(void) {
	
	return 0;
	
}

void dbgu_push(void *source, uint32_t length) {
	
	
	
}

void dbgu_pull(void *destination, uint32_t length) {
	
	
	
}