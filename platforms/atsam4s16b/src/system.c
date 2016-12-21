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

	/* Blink the LED for activity. */

	PMC -> PMC_PCER0 |= (1 << ID_PIOA);
	PIOA -> PIO_PER |= (1 << 0);
	PIOA -> PIO_OER |= (1 << 0);

	while (1) {
		PIOA -> PIO_SODR |= (1 << 0);
		for (uint32_t i = 0; i < 1000000; i ++) __asm__("nop");
		PIOA -> PIO_CODR |= (1 << 0);
		for (uint32_t i = 0; i < 1000000; i ++) __asm__("nop");
	}

	while (1);
}

void system_init(void) {
	/* Allow the reset pin to reset the device. */
	RSTC -> RSTC_MR |= RSTC_MR_URSTEN;
}

void system_deinit(void) {

}

/* Interrupt handler for this device driver. */
void UART0_handler(void) {

}
