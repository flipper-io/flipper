#include <flipper.h>

enum { __usart_read, __usart_write, _usart_get, _usart_put, _usart_ready, _usart_disable, _usart_enable, _usart_configure };

int _usart_read(void* dst, uint32_t length);
int _usart_write(void* src, uint32_t length);
uint8_t usart_get(void);
void usart_put(uint8_t byte);
int usart_ready(void);
void usart_disable(void);
void usart_enable(void);
int usart_configure(void);

void *usart_interface[] = {
	&_usart_read,
	&_usart_write,
	&usart_get,
	&usart_put,
	&usart_ready,
	&usart_disable,
	&usart_enable,
	&usart_configure
};

LF_MODULE(usart, "usart", usart_interface);

LF_WEAK int _usart_read(void* dst, uint32_t length) {
	lf_return_t retval;
	lf_invoke(lf_get_current_device(), "usart", __usart_read, lf_int_t, &retval, lf_args(lf_infer(dst), lf_infer(length)));
	return (int)retval;
}

LF_WEAK int _usart_write(void* src, uint32_t length) {
	lf_return_t retval;
	lf_invoke(lf_get_current_device(), "usart", __usart_write, lf_int_t, &retval, lf_args(lf_infer(src), lf_infer(length)));
	return (int)retval;
}

LF_WEAK uint8_t usart_get(void) {
	lf_return_t retval;
	lf_invoke(lf_get_current_device(), "usart", _usart_get, lf_int8_t, &retval, NULL);
	return (uint8_t)retval;
}

LF_WEAK void usart_put(uint8_t byte) {
	lf_return_t retval;
	lf_invoke(lf_get_current_device(), "usart", _usart_put, lf_void_t, &retval, lf_args(lf_infer(byte)));
	
}

LF_WEAK int usart_ready(void) {
	lf_return_t retval;
	lf_invoke(lf_get_current_device(), "usart", _usart_ready, lf_int_t, &retval, NULL);
	return (int)retval;
}

LF_WEAK void usart_disable(void) {
	lf_return_t retval;
	lf_invoke(lf_get_current_device(), "usart", _usart_disable, lf_void_t, &retval, NULL);
	
}

LF_WEAK void usart_enable(void) {
	lf_return_t retval;
	lf_invoke(lf_get_current_device(), "usart", _usart_enable, lf_void_t, &retval, NULL);
	
}

LF_WEAK int usart_configure(void) {
	lf_return_t retval;
	lf_invoke(lf_get_current_device(), "usart", _usart_configure, lf_int_t, &retval, NULL);
	return (int)retval;
}

