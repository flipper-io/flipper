#define __private_include__
#include <platform/atsam4s16b.h>

void system_init(void) {

	/* Allow the reset pin to reset the device. */
	RSTC -> RSTC_MR |= RSTC_MR_URSTEN;

}

void delay_ms() {
	uint64_t counter = 4000000;
	while (counter --) __asm__("nop");
}

void UART0_IrqHandler(void) {
	while (USART_IsDataAvailable((Usart *)UART0)) {
		USART_PutChar((Usart *)UART0, USART_GetChar((Usart *)UART0));
	}
}

void system_task(void) {

	/* Enable the UART0 clock in the PMC. */
	PMC_EnablePeripheral(ID_UART0);

	/* Declare a pin map that will configure the appropriate output pins for the UART0. */
	const Pin usart0_pins[] = { (Pin){ PIO_PA9A_URXD0 | PIO_PA10A_UTXD0, PIOA, ID_PIOA, PIO_PERIPH_A, PIO_DEFAULT } };
	/* Write the pinmap into the PIO. */
	PIO_Configure(usart0_pins, PIO_LISTSIZE(usart0_pins));
	/* Configure the UART0. */
	USART_Configure((Usart *)UART0, USART_MODE_ASYNCHRONOUS, 115200, BOARD_MCK);
	/* Enable the UART0 IRQ in the NVIC. */
	NVIC_EnableIRQ(UART0_IRQn);
	/* Enable the UART0 interrupt on receive. */
	USART_EnableIt((Usart *)UART0, UART_IER_RXRDY);
	/* Enable the UART0 transmitter. */
	USART_SetTransmitterEnabled((Usart *)UART0, 1);
	/* Enable the UART0 receiver. */
	USART_SetReceiverEnabled((Usart *)UART0, 1);

	PIOA -> PIO_PER |= (1 << 8);
	PIOA -> PIO_OER |= (1 << 8);
	PIOA -> PIO_SODR |= (1 << 8);
	PIOA -> PIO_CODR &= ~(1 << 8);

	const char dingas[] = "Hello world!";

	while (1) {
		PIOA -> PIO_SODR |= (1 << 8);
		PIOA -> PIO_CODR &= ~(1 << 8);
		delay_ms();
		PIOA -> PIO_SODR &= ~(1 << 8);
		PIOA -> PIO_CODR |= (1 << 8);
		delay_ms();
	}

}

void system_deinit(void) {

}
