#define __private_include__
#include <flipper/gpio.h>
#include <flipper/atmegau2/atmegau2.h>

int gpio_configure(void) {
	return lf_success;
}

void gpio_enable(uint32_t enable, uint32_t disable) {
	DDRD |= enable & 0xff;
	DDRD &= ~(disable & 0xff);
}

void gpio_write(uint32_t set, uint32_t clear) {
	printf("Writing GPIO s:0x%02x c:0x%02x\n", (uint8_t)(set & 0xFF), (uint8_t)(clear & 0xFF));
	PORTD |= set & 0xff;
	PORTD &= ~(clear & 0xff);
}

uint32_t gpio_read(uint32_t mask) {
	return (uint32_t)PIND;
}
