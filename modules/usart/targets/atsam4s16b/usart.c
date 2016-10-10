#define __private_include__
#include <flipper/usart.h>
#include <platform/atsam4s16b.h>

void usart_configure(void) {
	/* Enable the USART0 clock in the PMC. */
	PMC_EnablePeripheral(ID_USART0);
	/* Declare a pin map that will configure the appropriate output pins for the USART0. */
	const Pin usart0_pins[] = { (Pin){ PIO_PA9A_URXD0 | PIO_PA10A_UTXD0, PIOA, ID_PIOA, PIO_PERIPH_A, PIO_DEFAULT } };
	/* Write the pinmap into the PIO. */
	PIO_Configure(usart0_pins, PIO_LISTSIZE(usart0_pins));
	/* Configure the USART0. */
	USART_Configure((Usart *)USART0, USART_MODE_ASYNCHRONOUS, 115200, BOARD_MCK);
	/* Enable the USART0. */
	usart_enable();
}

void usart_enable(void) {
	/* Enable the USART0 IRQ in the NVIC. */
	NVIC_EnableIRQ(USART0_IRQn);
	/* Enable the USART0 interrupt on receive. */
	USART_EnableIt((Usart *)USART0, UART_IER_RXRDY);
	/* Enable the USART0 transmitter. */
	USART_SetTransmitterEnabled((Usart *)USART0, 1);
	/* Enable the USART0 receiver. */
	USART_SetReceiverEnabled((Usart *)USART0, 1);
}

void usart_disable(void) {
	/* Enable the USART0 IRQ in the NVIC. */
	NVIC_DisableIRQ(USART0_IRQn);
	/* Enable the USART0 interrupt on receive. */
	USART_DisableIt((Usart *)USART0, UART_IER_RXRDY);
	/* Enable the USART0 transmitter. */
	USART_SetTransmitterEnabled((Usart *)USART0, 0);
	/* Enable the USART0 receiver. */
	USART_SetReceiverEnabled((Usart *)USART0, 0);
}

uint8_t usart_ready(void) {
	return USART_IsDataAvailable((Usart *)USART0);
}

void usart_put(uint8_t byte) {
	USART_PutChar((Usart *)USART0, byte);
}


uint8_t usart_get(void) {
	return USART_GetChar((Usart *)USART0);
}

void usart_push(void *source, uint32_t length) {
	while (length --) usart_put(*(uint8_t *)(source ++));
	//USART_WriteBuffer((Usart *)USART0, source, length);
}

void usart_pull(void *destination, uint32_t length) {
	while (length --) *(uint8_t *)(destination ++) = usart_get();
	//USART_ReadBuffer((Usart *)USART0, destination, length);
}
