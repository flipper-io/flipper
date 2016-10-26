#define __private_include__
#include <flipper/uart0.h>
#include <platform/atsam4s16b.h>

int uart0_configure(void) {
	/* Enable the UART0 clock in the PMC. */
	PMC_EnablePeripheral(ID_UART0);
	/* Declare a pin map that will configure the appropriate output pins for the UART0. */
	const Pin uart0_pins[] = { (Pin){ PIO_PA9A_URXD0 | PIO_PA10A_UTXD0, PIOA, ID_PIOA, PIO_PERIPH_A, PIO_DEFAULT } };
	/* Write the pinmap into the PIO. */
	PIO_Configure(uart0_pins, PIO_LISTSIZE(uart0_pins));
	/* Configure the UART0. */
	USART_Configure((Usart *)UART0, USART_MODE_ASYNCHRONOUS, 250000, BOARD_MCK);
	/* Enable the UART0 interrupt on receive. */
	USART_EnableIt((Usart *)UART0, UART_IER_RXRDY | UART_IER_RXBUFF);
	/* Enable the UART0 transmitter. */
	USART_SetTransmitterEnabled((Usart *)UART0, 1);
	/* Enable the UART0 receiver. */
	USART_SetReceiverEnabled((Usart *)UART0, 1);
	/* Disable the UART0 IRQ in the NVIC and clear its interrupt state. */
	NVIC_DisableIRQ(UART0_IRQn);
	NVIC_ClearPendingIRQ(UART0_IRQn);
	NVIC_SetPriority(UART0_IRQn, 0);
	/* Enable the UART0 IRQ in the NVIC. */
	NVIC_EnableIRQ(UART0_IRQn);
	return lf_success;
}

void uart0_enable(void) {
	/* Enable the UART0 transmitter. */
	USART_SetTransmitterEnabled((Usart *)UART0, 1);
	/* Enable the UART0 receiver. */
	USART_SetReceiverEnabled((Usart *)UART0, 1);
}

void uart0_disable(void) {
	/* Enable the UART0 IRQ in the NVIC. */
	NVIC_DisableIRQ(UART0_IRQn);
	/* Enable the UART0 interrupt on receive. */
	USART_DisableIt((Usart *)UART0, UART_IER_RXRDY);
	/* Enable the UART0 transmitter. */
	USART_SetTransmitterEnabled((Usart *)UART0, 0);
	/* Enable the UART0 receiver. */
	USART_SetReceiverEnabled((Usart *)UART0, 0);
}

uint8_t uart0_ready(void) {
	return USART_IsDataAvailable((Usart *)UART0);
}

void uart0_put(uint8_t byte) {
	USART_PutChar((Usart *)UART0, byte);
}


uint8_t uart0_get(void) {
	return USART_GetChar((Usart *)UART0);
}

int uart0_push(void *source, lf_size_t length) {
	while (length --) uart0_put(*(uint8_t *)(source ++));
	return lf_success;
	//USART_WriteBuffer((Usart *)UART0, source, length);
}

int uart0_pull(void *destination, lf_size_t length) {
	//while (length --) *(uint8_t *)(destination ++) = uart0_get();
	USART_ReadBuffer((Usart *)UART0, destination, length);
	return lf_success;
}
