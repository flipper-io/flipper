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
	uart0_pull((void *)(&packet), sizeof(struct _fmr_packet));
	/* Wait for the data to be recieved by the PDC. */
	while (!(UART0 -> UART_SR & UART_SR_RXBUFF));
	/* Send over the USART bus for debugging. */
	usart_push(&packet, sizeof(packet));
	/* Flush the remaining bytes from the buffer. */
	while (uart0_ready()) {
		uart0_get();
	}
	/* If the packet has the correct magic number, try to execute the packet. */
	if (packet.header.magic == 0xFE) {
		struct _fmr_result result;
		fmr_perform(&packet, &result);
		/* Send the packet back to the host. */
		uart0_push(&result, sizeof(struct _fmr_result));
		//usart_push(&result, sizeof(struct _fmr_result));
		/* Clear any error state generated. */
		error_clear();
	}
}

void delay_ms() {
	uint64_t counter = 4000000;
	while (counter --) __asm__("nop");
}

void system_task(void) {

	// gpio_enable(PIO_PA8, PIO_DEFAULT);
	//
	// while (1) {
	// 	gpio_write(PIO_PA8, 1);
	// 	delay_ms();
	// 	gpio_write(PIO_PA8, 0);
	// 	delay_ms();
	// }

	while (1);

}

void system_init(void) {
	/* Allow the reset pin to reset the device. */
	RSTC -> RSTC_MR |= RSTC_MR_URSTEN;
	/* Disable the watchdog timer. */
	WDT_Disable(WDT);
	/* Configure the UART0 peripheral. */
	uart0_configure();
	/* Configure the USART0 peripheral. */
	usart_configure();
	/* Configure the GPIO peripheral. */
	//gpio_configure();
	/* Print the configuration. */
	//usart_push(self.configuration.name, strlen(self.configuration.name));
}

void system_deinit(void) {

}
