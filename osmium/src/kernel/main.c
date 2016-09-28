#define __private_include__
#include <osmium.h>
#include <flipper/modules.h>
#include <flipper/uart.h>
#include <private/nvm.h>

int main() {

	/* Initialize the platform hardware. */
	system_init();

	/* Initialize peripheral hardware. */
	button_configure();
	led_configure();
	led_set_rgb(LED_COLOR_SUCCESS);

	/* Clear the error state. */
	error_clear();

	spi_configure();
	nvm_configure();
	char out[] = "butts";
	nvm_push(out, sizeof(out), 0);
	char in[sizeof(out)];
	nvm_pull(in, sizeof(in), 0);
	if (memcmp(out, in, sizeof(out))) {
		error_raise(E_CHECKSUM, NULL);
		led_set_rgb(LED_COLOR_ERROR);
	}

	/* Perform the system task. */
	system_task();

	/* Loop here if the kernel were ever to reach an unknown execution state. */
	while (1);
}
