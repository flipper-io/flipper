#include <flipper/libflipper.h>

#ifdef __use_uart0__
#define __private_include__
#include <flipper/uart0.h>

int uart0_configure(void *_configuration) {
	printf("Configuring the uart0.\n");
	return lf_success;
}

bool uart0_ready(struct _lf_endpoint *self) {
	printf("Checking if the uart0 is ready.\n");
	return lf_success;
}

int uart0_push(void *source, lf_size_t length) {
	printf("Pushing to the uart0 bus.\n");
	return lf_success;
}

int uart0_pull(void *destination, lf_size_t length) {
	printf("Pulling from the uart0 bus.\n");
	return lf_success;
}

#endif
