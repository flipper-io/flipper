#define __private_include__
#include <flipper/gpio.h>
#include <flipper/uart0.h>
#include <flipper/modules.h>
#include <platform/atsam4s16b.h>

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
	PIOA -> PIO_OWER = PIO_PA0;

	/* ~ Configure the USART peripheral. ~ */
	usart_configure();

	/* ~ Configure the timer/counter peripheral. */
	// timer_configure();

	usart_push("Hello world!\n\n", 12);
	while (1) {
		PIOA -> PIO_ODSR ^= PIO_PA0;
		for (int i = 0; i < 10000000; i ++);
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
	RSTC -> RSTC_MR = RSTC_MR_KEY(0xA5) | RSTC_MR_URSTEN;
}

void system_deinit(void) {

}
