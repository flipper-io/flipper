#include <flipper.h>

enum { _uart0_pull, _uart0_push, _uart0_get, _uart0_put, _uart0_ready, _uart0_configure };

int uart0_pull(void* destination, lf_size_t length);
int uart0_push(void* source, lf_size_t length);
uint8_t uart0_get(void);
void uart0_put(uint8_t byte);
int uart0_ready(void);
int uart0_configure(void);

void *uart0_interface[] = {
	&uart0_pull,
	&uart0_push,
	&uart0_get,
	&uart0_put,
	&uart0_ready,
	&uart0_configure
};

LF_MODULE(uart0, "uart0", uart0_interface);

LF_WEAK int uart0_pull(void* destination, lf_size_t length) {
	return lf_pull(lf_get_current_device(), "uart0", _uart0_pull, destination, length, lf_args(lf_infer(destination), lf_infer(length)));
}

LF_WEAK int uart0_push(void* source, lf_size_t length) {
	return lf_push(lf_get_current_device(), "uart0", _uart0_push, source, length, lf_args(lf_infer(source), lf_infer(length)));
}

LF_WEAK uint8_t uart0_get(void) {
	return lf_invoke(lf_get_current_device(), "uart0", _uart0_get, lf_void_t, NULL);
}

LF_WEAK void uart0_put(uint8_t byte) {
	lf_invoke(lf_get_current_device(), "uart0", _uart0_put, lf_void_t, lf_args(lf_infer(byte)));
}

LF_WEAK int uart0_ready(void) {
	return lf_invoke(lf_get_current_device(), "uart0", _uart0_ready, lf_int32_t, NULL);
}

LF_WEAK int uart0_configure(void) {
	return lf_invoke(lf_get_current_device(), "uart0", _uart0_configure, lf_int32_t, NULL);
}

