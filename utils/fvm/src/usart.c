#include <flipper/libflipper.h>

#ifdef __use_usart__
#define __private_include__
#include <flipper/usart.h>

int usart_configure(void) {
	printf("Configuring the usart.\n");
	return lf_success;
}

void usart_enable(void) {
	printf("Enabling the usart.\n");
}

void usart_disable(void) {
	printf("Disabling the usart.\n");
}

uint8_t usart_ready(void) {
	printf("Checking if the usart is ready.\n");
	return lf_success;
}

void usart_put(uint8_t byte) {
	printf("Putting to the usart bus.\n");
}

uint8_t usart_get(void) {
	printf("Getting from the usart bus.\n");
	return lf_success;
}

int usart_push(void *source, lf_size_t length) {
	printf("Pushing to the usart bus.\n");
	return lf_success;
}

int usart_pull(void *destination, lf_size_t length) {
	printf("Pulling from the usart bus.\n");
	return lf_success;
}

#endif
