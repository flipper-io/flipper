#include <flipper.h>

enum { __usart_read, __usart_write, _usart_get, _usart_put, _usart_ready, _usart_disable, _usart_enable, _usart_configure };

int _usart_read(void* destination, uint32_t length);
int _usart_write(void* source, uint32_t length);
uint8_t usart_get(void);
void usart_put(uint8_t byte);
int usart_ready(void);
void usart_disable(void);
void usart_enable(void);
int usart_configure(void);

void *usart_interface[] = {
	&usart_read,
	&usart_write,
	&usart_get,
	&usart_put,
	&usart_ready,
	&usart_disable,
	&usart_enable,
	&usart_configure
};

LF_MODULE(usart, "usart", usart_interface);

LF_WEAK int usart_read(void* destination, uint32_t length) {
	return lf_success;
}

LF_WEAK int usart_write(void* source, uint32_t length) {
	return lf_success;
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
