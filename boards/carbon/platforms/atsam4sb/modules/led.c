#define __private_include__
#include <flipper/carbon/modules/led.h>
#include <flipper/carbon/platforms/atsam4s16b.h>

int led_configure(void) {
	return lf_success;
}

void led_set_rgb(uint8_t r, uint8_t g, uint8_t b) {

}
