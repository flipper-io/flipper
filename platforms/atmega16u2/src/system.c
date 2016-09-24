#define __private_include__
#include <platform/atmega16u2.h>
#include <private/usb.h>
#include <flipper/uart.h>
#include <flipper/led.h>

#define cpu_prescale(clock) (CLKPR = 0x80, CLKPR = clock)

uint32_t ret32(uint32_t val) {
	return val;
}

void system_task(void) {
	while (1) {
		struct _fmr_packet packet;
		int8_t _e = interrupt_receive_packet((void *)(&packet));
		if (_e > 0) {
			/* Calculate the number of bytes needed to encode the widths of the types. */
			uint8_t encode_length = lf_ceiling((packet.target.argc * 2), 8);
			/* Create a buffer for encoding argument types. */
			uint32_t types = 0;
			/* Copy the encoded type widths into the packet. */
			memcpy(&types, (void *)(&(packet.body)), encode_length);
			/* Call the function. */
			fmr_arg result = fmr_call(&ret32, packet.target.argc, (uint16_t)types, packet.body + encode_length);
			struct _fmr_result _result;
			/* This architecture is a little endian system, so we convert the endianess of the return type before sending it back to the host. */
			_result.value = result;
			_result.error = E_OVERFLOW;
			memcpy(&packet, &_result, sizeof(struct _fmr_result));
			interrupt_transmit_packet(&_result);
		}
	}
}

void system_init() {
	/* Prescale CPU for maximum clock. */
	cpu_prescale(0);
	/* Configure the USB subsystem. */
	configure_usb();
	/* Configure the UART subsystem. */
	uart_configure();
	/* Configure reset button and PCINT8 interrupt. */
	DDRC &= ~(1 << 6);
	PCMSK1 |= (1 << PCINT8);
	PCICR |= (1 << PCIE1);
	sei();
	/* Configure the device properties of the current target. */

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
