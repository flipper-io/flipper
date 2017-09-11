#define __private_include__
#include <osmium.h>
#include <flipper/nvm.h>
#include <scheduler.h>

int main() {

	/* Initialize the platform hardware. */
	system_init();

	/* Initialize peripheral hardware. */
	button_configure();
	led_configure();
	led_rgb(LED_COLOR_SUCCESS);
	// spi_configure();
	// nvm_configure();

	/* Clear the error state. */
	lf_error_clear();

	/* Initialize the scheduler and start executing the system task. */
	os_task_init();

	/* Loop here if the kernel were ever to reach an unknown execution state. */
	while (1);
}
