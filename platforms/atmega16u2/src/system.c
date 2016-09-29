#define __private_include__
#include <platform/atmega16u2.h>
#include <private/megausb.h>
#include <flipper/uart.h>
#include <flipper/cpu.h>
#include <flipper/led.h>

/* The fmr_device object containing global state about this device. */
struct _lf_device self = {
	{
		"flipper",
		0xc713,
		LF_VERSION,
		(lf_device_16bit | lf_device_little_endian)
	},
	&megausb,
	E_OK,
	false,
	NULL
};

#define cpu_prescale(clock) (CLKPR = 0x80, CLKPR = clock)

/* Helper functions to libflipper. */

void fmr_push(fmr_module module, fmr_function function, lf_size_t length) {
	void *swap = malloc(length);
	if (!swap) {
		error_raise(E_MALLOC, NULL);
		return;
	}
	megausb_pull(swap, length);
	/* Call the function. */
	const void *address = lf_std_function(module, function);
	((void (*)(void *, uint32_t))address)(swap, length);
	free(swap);
}

void fmr_pull(fmr_module module, fmr_function function, lf_size_t length) {
	void *swap = malloc(length);
	if (!swap) {
		error_raise(E_MALLOC, NULL);
		return;
	}
	/* Call the function. */
	const void *address = lf_std_function(module, function);
	((void (*)(void *, uint32_t))address)(swap, length);
	megausb_push(swap, length);
	free(swap);
}

struct _lf_configuration *system_configuration(void) {
	return &self.configuration;
}

void system_task(void) {
	while (1) {

#if 0
		/* bulk transfer tests. */
		char buffer[FMR_PACKET_SIZE];
		if (bulk_receive_packet(buffer) > 0) {
			led_set_rgb(buffer[0], buffer[1], buffer[2]);
			buffer[0] = 0x0a;
			buffer[1] = 0x0b;
			buffer[2] = 0x0c;
			bulk_transmit_packet(buffer);
		}
#else
		/* FMR */
		struct _fmr_packet packet;
		int8_t _e = megausb_pull((void *)(&packet), sizeof(struct _fmr_packet));
		if (!(_e < lf_success)) {
			/* Create a buffer to the result of the operation. */
			struct _fmr_result result;
			/* If the host is asking for the device attributes, return them. */
			if (packet.target.attributes & LF_CONFIGURATION) {
				/* Send the configuration information back to the host. */
				megausb_push(system_configuration(), sizeof(struct _lf_configuration));
			} else {
				/* Execute the packet. */
				fmr_perform(&packet, &result);
				/* Send the result back to the host. */
				megausb_push(&result, sizeof(struct _fmr_result));
			}
			/* Clear any error state generated by the procedure. */
			error_clear();
		}
#endif
	}
}

void system_init() {
	/* Prescale CPU for maximum clock. */
	cpu_prescale(0);
	/* Configure the USB subsystem. */
	configure_usb();
	/* Configure the UART subsystem. */
	uart_configure();
	/* Configure the SAM4S. */
	cpu_configure();
	/* Configure reset button and PCINT8 interrupt. */
	PCMSK1 |= (1 << PCINT8);
	PCICR |= (1 << PCIE1);
	sei();
}

void system_deinit(void) {
	/* Clear the state of the status LED. */
	led_set_rgb(LED_OFF);
}

/* PCINT8 interrupt service routine; captures reset button press and resets device. */
ISR (PCINT1_vect) {
	/* Shutdown the system. */
	system_deinit();
	/* Enable the watchdog. */
	wdt_enable(WDTO_15MS);
	/* Wait until reset. */
	while (1);
}
