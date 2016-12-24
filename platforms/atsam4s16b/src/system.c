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

char incoming[4];
/* Create an outgoing DMA transfer. */
char outgoing[] = "LEONARD EULER";

void system_task(void) {

	/* ~ Configure the LED that exists on PA0. ~ */
	PMC -> PMC_PCER0 |= (1 << ID_PIOA);
	PIOA -> PIO_PER |= PIO_PA0;
	PIOA -> PIO_OER |= PIO_PA0;

	/* ~ Configure the USART peripheral. ~ */
	usart_configure();
	/* Enable the USART0 interrupt. */
	NVIC_EnableIRQ(USART0_IRQn);

	/* Reset the transmitter. */
	USART0 -> US_PTCR = US_PTCR_TXTDIS;
	/* Enable the PDC transmit interrupt. */
	USART0 -> US_IER = US_IER_ENDTX;
	/* Set both transmitter channels. */
	USART0 -> US_TCR = sizeof(outgoing);
	USART0 -> US_TPR = outgoing;
	USART0 -> US_TNCR = 0;
	USART0 -> US_TNPR = NULL;
	/* Enable the transmitter. */
	USART0 -> US_PTCR = US_PTCR_TXTEN;

	/* Reset the receiver. */
	USART0 -> US_PTCR = US_PTCR_RXTDIS;
	/* Enable the PDC receive interrupt. */
	USART0 -> US_IER = US_IER_ENDRX;
	/* Clear both receiver channels. */
	USART0 -> US_RCR = 0;
	USART0 -> US_RPR = NULL;
	USART0 -> US_RNCR = 0;
	USART0 -> US_RNPR = NULL;

	/* ~ Configure the timer/counter peripheral. */

	// PMC -> PMC_PCER0 |= (1 << ID_TC0);
	// TC0 -> TC_CHANNEL[0].TC_CCR |= TC_CCR_CLKDIS;
	// TC0 -> TC_CHANNEL[0].TC_IER = 0;


	while (1) {
		PIOA -> PIO_SODR |= PIO_PA0;
		for (uint32_t i = 0; i < 10000000; i ++) __asm__("nop");
		PIOA -> PIO_CODR |= PIO_PA0;
		for (uint32_t i = 0; i < 10000000; i ++) __asm__("nop");
	}

}

void usart0_isr(void) {
	if (USART0 -> US_CSR & US_CSR_ENDRX) {
		/* Disable the receiver. */
		USART0 -> US_PTCR = US_PTCR_RXTDIS;
		/* A non-zero value needs to be written here to clear the interrupt flag? */
		USART0 -> US_RCR = 1;

		/* Copy the incoming data to the outging data. */
		memcpy(outgoing, incoming, sizeof(outgoing));

		/* Start an outgoing DMA transfer. */
		USART0 -> US_TCR = sizeof(outgoing);
		USART0 -> US_TPR = outgoing;
		/* Enable the transmitter. */
		USART0 -> US_PTCR = US_PTCR_TXTEN;

		usart_put('*');
	} else if (USART0 -> US_CSR & US_CSR_ENDTX) {
		/* Disable the transmitter. */
		USART0 -> US_PTCR = US_PTCR_TXTDIS;
		/* A non-zero value needs to be written here to clear the interrupt flag? */
		USART0 -> US_TCR = 1;

		/* Create an incoming DMA transfer. */
		USART0 -> US_RCR = sizeof(incoming);
		USART0 -> US_RPR = incoming;
		/* Enable the receiver. */
		USART0 -> US_PTCR = US_PTCR_RXTEN;

		usart_put('!');
	} else {
		usart_put('?');
	}
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
