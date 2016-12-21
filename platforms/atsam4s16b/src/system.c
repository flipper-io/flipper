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

struct _fmr_packet packet;

/* Interrupt handler for this device driver. */
void UART0_IrqHandler(void) {

}

void delay_ms() {
	uint64_t counter = 4000000;
	while (counter --) __asm__("nop");
}

void system_task(void) {

	gpio_enable(PIO_PA0, PIO_DEFAULT);

	while (1) {
		gpio_write(PIO_PA0, 1);
		delay_ms();
		gpio_write(PIO_PA0, 0);
		delay_ms();

		// while (!uart0_ready());
		// uart0_pull((void *)(&packet), sizeof(struct _fmr_packet));
		// usart_push(&packet, sizeof(struct _fmr_packet));
	}

	while (1);

}

void system_init(void) {
	/* Allow the reset pin to reset the device. */
	RSTC -> RSTC_MR |= RSTC_MR_URSTEN;
	/* Disable the watchdog timer. */
	WDT_Disable(WDT);
	/* Configure the USART0 peripheral. */
	usart_configure();
	/* Configure the UART0 peripheral. */
	//uart0_configure();
	/* Configure the GPIO peripheral. */
	gpio_configure();
	/* Print the configuration. */
	//usart_push(self.configuration.name, strlen(self.configuration.name));
}

void system_deinit(void) {

}
