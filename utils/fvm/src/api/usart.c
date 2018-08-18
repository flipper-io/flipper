#include <flipper.h>

LF_FUNC("usart") int usart_configure(void) {
	printf("Configured the usart.\n");
	return lf_success;
}

LF_FUNC("usart") int usart_ready(void) {
	printf("Checking if the usart is ready.\n");
	return lf_success;
}

LF_FUNC("usart") int usart_push(void *src, uint32_t length) {
	printf("Pushing '%s' to the usart bus.\n", src);
	return lf_success;
}

LF_FUNC("usart") int usart_pull(void *dst, uint32_t length) {
	printf("Pulling from the usart bus.\n");
	return lf_success;
}
