#include <flipper/libflipper.h>

#ifdef __use_uart0__
#define __private_include__
#include <flipper/uart0.h>

int usart0_configure(void) {
	printf("Configuring the usart0.\n");
	return lf_success;
}

void usart0_enable(void) {
	printf("Enabling the usart0.\n");
}

void usart0_disable(void) {
	printf("Disabling the usart0.\n");
}

uint8_t usart0_ready(void) {
	printf("Checking if the usart0 is ready.\n");
	return lf_success;
}

void usart0_put(uint8_t byte) {
	printf("Putting to the usart0 bus.\n");
}

uint8_t usart0_get(void) {
	printf("Getting from the usart0 bus.\n");
	return lf_success;
}

int usart0_push(void *source, lf_size_t length) {
	printf("Pushing to the usart0 bus.\n");
	return lf_success;
}

int usart0_pull(void *destination, lf_size_t length) {
	printf("Pulling from the usart0 bus.\n");
	return lf_success;
}

#endif
