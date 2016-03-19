#define __private_include__
#include <usart/usart.h>
#include <platform.h>

void usart_configure(AT91S_USART *usart, uint16_t baud) {

	/* ~ Reset the USART controller and temporarily disable communications. ~ */
	set_bits_in_port_with_mask(usart -> US_CR, (AT91C_US_RSTRX | AT91C_US_RSTTX | AT91C_US_RXDIS | AT91C_US_TXDIS));

	/* ~ Configure the USART controller. (8N1) ~ */
	set_bits_in_port_with_mask(usart -> US_MR, (AT91C_US_USMODE_NORMAL | AT91C_US_CLKS_CLOCK | AT91C_US_CHRL_8_BITS | AT91C_US_PAR_NONE | AT91C_US_NBSTOP_1_BIT));

	/* ~ Set the baudrate. ~ */
	usart -> US_BRGR = baud;

	/* ~ Enable communications. ~ */
	set_bits_in_port_with_mask(usart -> US_CR, (AT91C_US_RXEN | AT91C_US_TXEN));

}

void usart_enable(AT91S_USART *usart) {

	set_bits_in_port_with_mask(usart -> US_CR, (AT91C_US_RXEN | AT91C_US_TXEN));

}

void usart_disable(AT91S_USART *usart) {

	set_bits_in_port_with_mask(usart -> US_CR, (AT91C_US_RXDIS | AT91C_US_TXDIS));

}

bool usart_ready(AT91S_USART *usart) {

	return (usart -> US_CSR & AT91C_US_RXRDY);

};

void usart_put_byte(AT91S_USART *usart, uint8_t byte) {

	/* ~ Wait until the USART is ready to transmit. ~ */
	while (!(usart -> US_CSR & AT91C_US_TXRDY));

	/* ~ Send the byte. ~ */
	usart -> US_THR = byte;

}

uint8_t usart_get_byte(AT91S_USART *usart) {

	while (!(usart -> US_CSR & AT91C_US_RXRDY));

	/* ~ Read the byte. ~ */
	return (usart -> US_RHR);

}

void usart_push(AT91S_USART *usart, void *source, size_t length) {

	/* ~ THIS SHOULD BE OPTIMIZED TO USE THE DMAC. ~ */
	while (length --) usart_put_byte(usart, *(uint8_t *)(source ++));

}

void usart_pull(AT91S_USART *usart, void *destination, size_t length) {

	/* ~ THIS SHOULD BE OPTIMIZED TO USE THE DMAC. ~ */
	while (length --) *(uint8_t *)(destination ++) = usart_get_byte(usart);

}

/* ~ ----------------------- USART0 ----------------------- ~ */

void usart0_configure(void *baud) {

	/* ~ Enable USART0 in the PCER (peripheral clock enable register). ~ */
	set_bit_in_port(AT91C_ID_US0, AT91C_BASE_PMC -> PMC_PCER);

	/* ~ Disable use of the RX and TX pins by the peripheral IO controller. They are now in use by the USART controller. ~ */
	set_bits_in_port_with_mask(AT91C_BASE_PIOA -> PIO_PDR, (AT91C_PA5_RXD0 | AT91C_PA6_TXD0));

	/* ~ Configure the USART hardware. ~ */
	usart_configure(AT91C_BASE_US0, (uint32_t)(baud));

}

void usart0_enable(void) {

	usart_enable(AT91C_BASE_US0);

}

void usart0_disable(void) {

	usart_disable(AT91C_BASE_US0);

}

bool usart0_ready(void) {

	return usart_ready(AT91C_BASE_US0);

}

void usart0_put(uint8_t byte) {

	usart_put_byte(AT91C_BASE_US0, byte);

}

uint8_t usart0_get(void) {

	return usart_get_byte(AT91C_BASE_US0);

}

void usart0_push(void *source, uint32_t length) {

	usart_push(AT91C_BASE_US0, source, length);

}

void usart0_pull(void *destination, uint32_t length) {

	usart_pull(AT91C_BASE_US0, destination, length);

}

/* ~ ----------------------- USART1 ----------------------- ~ */

void usart1_configure(void *baud) {

	/* ~ Enable USART1 in the PCER (peripheral clock enable register). ~ */
	set_bit_in_port(AT91C_ID_US1, AT91C_BASE_PMC -> PMC_PCER);

	/* ~ Disable use of the RX and TX pins by the peripheral IO controller. They are now in use by the USART controller. ~ */
	set_bits_in_port_with_mask(AT91C_BASE_PIOA -> PIO_PDR, (AT91C_PA21_RXD1 | AT91C_PA22_TXD1));

	/* ~ Configure the USART hardware. ~ */
	usart_configure(AT91C_BASE_US1, (uint32_t)(baud));

}

void usart1_enable(void) {

	usart_enable(AT91C_BASE_US1);

}

void usart1_disable(void) {

	usart_disable(AT91C_BASE_US1);

}

bool usart1_ready(void) {

	return usart_ready(AT91C_BASE_US1);

}

void usart1_put(uint8_t byte) {

	usart_put_byte(AT91C_BASE_US1, byte);

}

uint8_t usart1_get(void) {

	return usart_get_byte(AT91C_BASE_US1);

}

void usart1_push(void *source, uint32_t length) {

	usart_push(AT91C_BASE_US1, source, length);

}

void usart1_pull(void *destination, uint32_t length) {

	usart_pull(AT91C_BASE_US1, destination, length);

}

/* ~ ----------------------- DBGU ----------------------- ~ */

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
