#include <flipper.h>

LF_FUNC("uart0") int uart0_configure(void) {
	printf("Configured the uart0.\n");
	return lf_success;
}

LF_FUNC("uart0") int uart0_ready(void) {
	printf("Checking if the uart0 is ready.\n");
	return lf_success;
}

LF_FUNC("uart0") void uart0_enable(void) {
	printf("Enabling the uart0 bus.\n");
}

LF_FUNC("uart0") void uart0_disable(void) {
	printf("Disabling the uart0 bus.\n");
}

LF_FUNC("uart0") void uart0_put(uint8_t byte) {
	printf("Putting '%c' to the uart0 bus.\n", byte);
}

LF_FUNC("uart0") uint8_t uart0_get(void) {
	printf("Getting from the uart0 bus.\n");
	return '\0';
}

LF_FUNC("uart0") int uart0_write(void *src, size_t length) {
	printf("Writing to the uart0 bus: %s\n", src);
	return lf_success;
}

LF_FUNC("uart0") int uart0_read(void *dst, size_t length) {
	printf("Reading from the uart0 bus.\n");
	return lf_success;
}
