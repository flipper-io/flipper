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

/* Interrupt handler for this device driver. */
void UART0_IrqHandler(void) {
	struct _fmr_packet packet;
	uart0_pull(&packet, sizeof(packet));
	struct _fmr_result result;
	fmr_perform(&packet, &result);
	uart0_push(&result, sizeof(struct _fmr_result));
}

void delay_ms() {
	uint64_t counter = 4000000;
	while (counter --) __asm__("nop");
}

void system_task(void) {

	//gpio_enable(PIO_PA8, PIO_DEFAULT);

	while (1) {
		// gpio_write(PIO_PA8, 1);
		// delay_ms();
		// gpio_write(PIO_PA8, 0);
		// delay_ms();
	}

}

void system_init(void) {
	/* Allow the reset pin to reset the device. */
	RSTC -> RSTC_MR |= RSTC_MR_URSTEN;
	/* Configure the GPIO peripheral. */
	// const void *cfg = lf_std_function(_gpio_id, _gpio_configure);
	// ((void (*)(void))cfg)();
	/* Configure the UART0 peripheral. */
	//uart0_configure();
	
}

void system_deinit(void) {

}
