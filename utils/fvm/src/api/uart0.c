#include <flipper.h>

int uart0_configure(uint8_t baud, uint8_t interrupts) {
	printf("Configuring the uart0.\n");
	return lf_success;
}

int uart0_ready(void) {
	printf("Checking if the uart0 is ready.\n");
	return lf_success;
}

int uart0_push(void *source, lf_size_t length) {
	printf("Pushing to the uart0 bus: %s\n", source);
	return lf_success;
}

int uart0_pull(void *destination, lf_size_t length) {
	printf("Pulling from the uart0 bus.\n");
	return lf_success;
}
