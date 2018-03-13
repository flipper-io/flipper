#include <flipper.h>

int usart_configure(void) {
	printf("Configuring the usart.\n");
	return lf_success;
}

int usart_ready(void) {
	printf("Checking if the usart is ready.\n");
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
