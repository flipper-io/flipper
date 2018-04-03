#include <flipper.h>

enum { _usart_pull, _usart_push, _usart_get, _usart_put, _usart_ready, _usart_disable, _usart_enable, _usart_configure };

int usart_pull(void* destination, lf_size_t length);
int usart_push(void* source, lf_size_t length);
uint8_t usart_get(void);
void usart_put(uint8_t byte);
int usart_ready(void);
void usart_disable(void);
void usart_enable(void);
int usart_configure(void);

void *usart_interface[] = {
	&usart_pull,
	&usart_push,
	&usart_get,
	&usart_put,
	&usart_ready,
	&usart_disable,
	&usart_enable,
	&usart_configure
};

LF_MODULE(usart, "usart", usart_interface);

LF_WEAK int usart_pull(void* destination, lf_size_t length) {
	return lf_pull(lf_get_current_device(), "usart", _usart_pull, destination, length);
}

LF_WEAK int usart_push(void* source, lf_size_t length) {
	return lf_push(lf_get_current_device(), "usart", _usart_push, source, length);
}

LF_WEAK uint8_t usart_get(void) {
	return lf_invoke(lf_get_current_device(), "usart", _usart_get, lf_int8_t, NULL);
}

LF_WEAK void usart_put(uint8_t byte) {
	lf_invoke(lf_get_current_device(), "usart", _usart_put, lf_void_t, lf_args(lf_infer(byte)));
}

LF_WEAK int usart_ready(void) {
	return lf_invoke(lf_get_current_device(), "usart", _usart_ready, lf_int_t, NULL);
}

LF_WEAK void usart_disable(void) {
	lf_invoke(lf_get_current_device(), "usart", _usart_disable, lf_void_t, NULL);
}

LF_WEAK void usart_enable(void) {
	lf_invoke(lf_get_current_device(), "usart", _usart_enable, lf_void_t, NULL);
}

LF_WEAK int usart_configure(void) {
	return lf_invoke(lf_get_current_device(), "usart", _usart_configure, lf_int_t, NULL);
}

