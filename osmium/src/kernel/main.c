#define __private_include__
#include <osmium.h>
#include <flipper/led.h>

int main() {

	/* Initialize the platform hardware. */
	system_init();

	led_configure();
	led_set_rgb(0, 0, 100);

	/* Loop here if the kernel were ever to reach an unknown execution state. */
	while (1);

}
