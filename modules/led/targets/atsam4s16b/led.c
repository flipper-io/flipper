#define __private_include__
#include <flipper/led.h>
#include <platforms/atsam4s16b.h>

int led_configure(void) {
	return lf_success;
}

void led_set_rgb(uint8_t r, uint8_t g, uint8_t b) {

}
