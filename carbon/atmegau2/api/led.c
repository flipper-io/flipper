#include <flipper/led.h>

LF_FUNC("led") int led_configure(void) {
	/* Configure the DI (data in) pin of the LED as an output. */
	LED_DDR |= (1 << LED_DI);
	return lf_success;
}

LF_FUNC("led") void led_rgb(uint8_t r, uint8_t g, uint8_t b) {

	/* Create an array to be sent to the LED in GRB format. */
	uint8_t *data = (uint8_t *)&((uint8_t []){ g, r, b });

	/* Send each of the three bytes to the LED, one by one. */
	for (uint8_t i = 3; i > 0; i --) {

		uint8_t dummy, byte = *data++;

		/* Let the bit-banging begin. */
		__asm__ volatile ("ldi %0,8     \n\t"
						  "loop%=:out %2,%3 \n\t"
						  "lsl %1           \n\t"
						  "dec %0           \n\t"
						  "rjmp .+0         \n\t"
						  "brcs .+2         \n\t"
						  "out %2,  %4      \n\t"
						  "rjmp .+0         \n\t"
						  "nop              \n\t"
						  "out %2,  %4      \n\t"
						  "breq end%=       \n\t"
						  "rjmp .+0         \n\t"
						  "rjmp .+0         \n\t"
						  "rjmp .+0         \n\t"
						  "rjmp loop%=      \n\t"
						  "end%=:           \n\t" :
						  "=&d" (dummy)           :
						  "r" (byte),
						  "I" (_SFR_IO_ADDR(LED_PORT)),
						  "r" (PORTB | (1 << LED_DI)),
						  "r" (~(1 << LED_DI) & LED_PORT));

	}

}
