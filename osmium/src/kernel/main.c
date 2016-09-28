#define __private_include__
#include <osmium.h>
#include <flipper/modules.h>
#include <flipper/uart.h>

int main() {

	/* Initialize the platform hardware. */
	system_init();

	/* Initialize peripheral hardware. */
	button_configure();
	led_configure();
	led_set_rgb(LED_COLOR_SUCCESS);

	/* Clear the error state. */
	error_clear();

	// spi_configure();
	// fs_configure();

	/* Perform the system task. */
	system_task();

	/* Loop here if the kernel were ever to reach an unknown execution state. */
	while (1);
}
