#define __private_include__
#include <flipper/gpio.h>
#include <flipper/uart0.h>
#include <flipper/modules.h>
#include <platform/atsam4s16b.h>

#include <cmsis/core_cm3.h>

/* The fmr_device object containing global state about this device. */
struct _lf_device self = {
	{
		"flipper",
		0xc713,
		LF_VERSION,
		(lf_device_32bit | lf_device_little_endian)
	},
	NULL,
	E_OK,
	false,
	NULL
};

/* Helper functions to libflipper. */

void fmr_push(fmr_module module, fmr_function function, lf_size_t length) {

}

void fmr_pull(fmr_module module, fmr_function function, lf_size_t length) {

}

void system_task(void) {

	/* ~ Configure the LED that exists on PA0. ~ */
	PMC -> PMC_PCER0 |= (1 << ID_PIOA);
	PIOA -> PIO_PER |= PIO_PA0;
	PIOA -> PIO_OER |= PIO_PA0;

	/* ~ Configure the USART peripheral. ~ */

	/* Create a pinmask for the peripheral pins. */
	const unsigned int USART0_PIN_MASK = (PIO_PA5A_RXD0 | PIO_PA6A_TXD0);
	/* Enable the peripheral clock. */
	PMC -> PMC_PCER0 |= (1 << ID_USART0);
	/* Disable PIOA interrupts on the peripheral pins. */
	PIOA -> PIO_IDR |= USART0_PIN_MASK;
	/* Disable the peripheral pins from use by the PIOA. */
	PIOA -> PIO_PDR |= USART0_PIN_MASK;
	/* Hand control of the peripheral pins to peripheral A. */
	PIOA -> PIO_ABCDSR[0] &= ~USART0_PIN_MASK;
	PIOA -> PIO_ABCDSR[1] &= ~USART0_PIN_MASK;
	/* Reset the peripheral and disable the transmitter and receiver. */
	USART0 -> US_CR = UART_CR_RSTRX | UART_CR_RSTTX | UART_CR_TXDIS | UART_CR_RXDIS;
	/* Set the mode to 8n1. */
	USART0 -> US_MR = US_MR_CHRL_8_BIT | US_MR_PAR_NO | US_MR_NBSTOP_1_BIT;
	/* Set the baudrate. */
	USART0 -> US_BRGR = (F_CPU / PLATFORM_BAUDRATE / 16);
	/* Enable the character receive interrupt. */
	USART0 -> US_IER = US_IER_RXRDY;
	/* Enable the transmitter and receiver. */
	USART0 -> US_CR = UART_CR_TXEN | UART_CR_RXEN;
	/* Enable the USART0 interrupt. */
	NVIC_EnableIRQ(USART0_IRQn);

	/* ~ Configure the timer/counter peripheral. */

	PMC -> PMC_PCER0 |= (1 << ID_TC0);
	TC0 -> TC_CHANNEL[0].TC_CCR |= TC_CCR_CLKDIS;
	TC0 -> TC_CHANNEL[0].TC_IER = 0;


	while (1) {
		PIOA -> PIO_SODR |= PIO_PA0;
		for (uint32_t i = 0; i < 10000000; i ++) __asm__("nop");
		PIOA -> PIO_CODR |= PIO_PA0;
		for (uint32_t i = 0; i < 10000000; i ++) __asm__("nop");
	}

}

void usart0_isr(void) {
	/* Wait until USART0 is ready to transmit. */
	while (!(USART0 -> US_CSR & US_CSR_TXEMPTY));
	/* Write the character into the transmit buffer. */
	USART0 -> US_THR = USART0 -> US_RHR;
}

void system_init(void) {
	uint32_t timeout;

	/* Configure the EFC for 3 wait states. */
	EFC -> EEFC_FMR = EEFC_FMR_FWS(3);

	/* Configure the primary clock source. */
	if (!(PMC -> CKGR_MOR & CKGR_MOR_MOSCSEL)) {
		PMC -> CKGR_MOR = CKGR_MOR_KEY(0x37) | BOARD_OSCOUNT | CKGR_MOR_MOSCRCEN | CKGR_MOR_MOSCXTEN;
		for (timeout = 0; !(PMC -> PMC_SR & PMC_SR_MOSCXTS) && (timeout ++ < CLOCK_TIMEOUT););
	}

	/* Select external 20MHz oscillator. */
	PMC -> CKGR_MOR = CKGR_MOR_KEY(0x37) | BOARD_OSCOUNT | CKGR_MOR_MOSCRCEN | CKGR_MOR_MOSCXTEN | CKGR_MOR_MOSCSEL;
	for (timeout = 0; !(PMC -> PMC_SR & PMC_SR_MOSCSELS) && (timeout ++ < CLOCK_TIMEOUT););
	PMC -> PMC_MCKR = (PMC -> PMC_MCKR & ~(uint32_t)PMC_MCKR_CSS_Msk) | PMC_MCKR_CSS_MAIN_CLK;
	for (timeout = 0; !(PMC -> PMC_SR & PMC_SR_MCKRDY) && (timeout++ < CLOCK_TIMEOUT););

	/* Configure PLLB as the master clock PLL. */
	PMC -> CKGR_PLLBR = BOARD_PLLBR;
	for (timeout = 0; !(PMC -> PMC_SR & PMC_SR_LOCKB) && (timeout++ < CLOCK_TIMEOUT););

	/* Switch to the main clock. */
	PMC -> PMC_MCKR = (BOARD_MCKR & ~PMC_MCKR_CSS_Msk) | PMC_MCKR_CSS_MAIN_CLK;
	for (timeout = 0; !(PMC -> PMC_SR & PMC_SR_MCKRDY) && (timeout++ < CLOCK_TIMEOUT););
	PMC -> PMC_MCKR = BOARD_MCKR;
	for (timeout = 0; !(PMC -> PMC_SR & PMC_SR_MCKRDY) && (timeout++ < CLOCK_TIMEOUT););

	/* Allow the reset pin to reset the device. */
	RSTC -> RSTC_MR |= RSTC_MR_URSTEN;
}

void system_deinit(void) {

}
