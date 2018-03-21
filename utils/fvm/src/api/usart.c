#include <flipper.h>

extern struct _lf_module usart;

LF_FUNC("usart") int usart_configure(void) {
	dyld_register(&THIS_DEVICE, &usart);
	printf("Configured the usart.\n");
	return lf_success;
}

LF_FUNC("usart") int usart_ready(void) {
	printf("Checking if the usart is ready.\n");
	return lf_success;
}

LF_FUNC("usart") int usart_push(void *source, lf_size_t length) {
	printf("Pushing to the usart bus.\n");
	return lf_success;
}

LF_FUNC("usart") int usart_pull(void *destination, lf_size_t length) {
	printf("Pulling from the usart bus.\n");
	return lf_success;
}
