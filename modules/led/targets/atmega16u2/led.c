#define __private_include__
#include <flipper/led.h>
#include <platform/atmega16u2.h>

int led_configure(void) {
	/* Configure the DI (data in) pin of the LED as an output. */
	set_bit_in_port(LED_DI, LED_DDR);
	return lf_success;
}


uint8_t r_c, g_c, b_c;
int8_t direction = 1;
uint8_t ticks;

void led_pulse(uint8_t r, uint8_t g, uint8_t b) {
	TCCR1B |= (1 << WGM12);
	OCR1A = 15625;
	TIMSK1 |= (1 << OCIE1A);
	TCCR1B |= (0 << CS12) | (1 << CS11) | (0 << CS10);
	r_c = r;
	g_c = g;
	b_c = b;
	direction = 1;
}

ISR(TIMER1_COMPA_vect) {
	//r_c += direction;
	//g_c += direction;
	b_c += direction;
	led_set_rgb(r_c, g_c, b_c);
	if (b_c >= 128) {
		direction = -1;
	} else if (b_c == 1) {
		direction = 1;
	}
}

void led_set_rgb(uint8_t r, uint8_t g, uint8_t b) {

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
