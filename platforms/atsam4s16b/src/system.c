#define __private_include__
#include <platform/atsam4s16b.h>

void system_init(void) {

	/* Allow the reset pin to reset the device. */
	RSTC -> RSTC_MR |= RSTC_MR_URSTEN;

}

void delay_ms() {
	uint64_t counter = 1000000;
	while (counter --) __asm__("nop");
}

void system_task(void) {

	PMC_EnablePeripheral(ID_USART0);
	const Pin usart0_pins[] = { (Pin){ PIO_PA5A_RXD0 | PIO_PA6A_TXD0, PIOA, ID_PIOA, PIO_PERIPH_A, PIO_DEFAULT } };
	PIO_Configure(usart0_pins, PIO_LISTSIZE(usart0_pins));
	USART_Configure(USART0, USART_MODE_ASYNCHRONOUS, 115200, BOARD_MCK);
	USART_SetTransmitterEnabled(USART0, 1);

	PIOA -> PIO_PER |= (1 << 8);
	PIOA -> PIO_OER |= (1 << 8);

	const char *dingas = "Hello world!";

	while (1) {
		USART_WriteBuffer(USART0, dingas, 11);
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
