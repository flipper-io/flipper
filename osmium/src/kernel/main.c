#define __private_include__
#include <osmium.h>
#include <flipper/modules.h>

int main() {

	/* Initialize the platform hardware. */
	system_init();

	/* Initialize the status LED hardware. */
	led_configure();
	led_set_rgb(LED_COLOR_SUCCESS);

	/* Perform the system task. */
	system_task();

	/* Loop here if the kernel were ever to reach an unknown execution state. */
	while (1);
}
