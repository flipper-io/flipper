#include <flipper.h>

enum { _uart0_read, _uart0_write, _uart0_get, _uart0_put, _uart0_ready, _uart0_reset, _uart0_setbaud, _uart0_configure, _uart0_enable };

int uart0_read(void* dst, uint32_t length);
int uart0_write(void* src, uint32_t length);
uint8_t uart0_get(void);
void uart0_put(uint8_t byte);
int uart0_ready(void);
int uart0_reset(void);
int uart0_setbaud(uint32_t baud);
int uart0_configure(void);
void uart0_enable(void);

void *uart0_interface[] = {
	&uart0_read,
	&uart0_write,
	&uart0_get,
	&uart0_put,
	&uart0_ready,
	&uart0_reset,
	&uart0_setbaud,
	&uart0_configure,
	&uart0_enable
};

LF_MODULE(uart0, "uart0", uart0_interface);

#warning Memory not freed if invoke or pull fail.
LF_WEAK int uart0_read(void* destination, uint32_t length) {
	void *buffer = NULL;
	lf_return_t retval;
	struct _lf_device *device = lf_get_selected();
	lf_assert(device, fail, E_UNIMPLEMENTED, "NULL device for uart0_read.");
	lf_assert(lf_malloc(device, length, &buffer) == lf_success, fail, E_NULL, "Failed to allocate remote memory for uart0_read.");
	lf_assert(lf_invoke(device, "uart0", _uart0_read, lf_int_t, &retval, lf_args(lf_ptr(buffer), lf_infer(length))) == lf_success, fail, E_UNIMPLEMENTED, "Failed to invoke uart0_read.");
	lf_assert(lf_pull(device, destination, buffer, length) == lf_success, fail, E_UNIMPLEMENTED, "Failed to pull uart0.");
	lf_assert(lf_free(device, buffer) == lf_success, fail, E_UNIMPLEMENTED, "Failed to free uart0_read memory.");
	return (int)retval;
fail:
	return lf_error;
}

#warning Memory not freed if push or invoke fail.
LF_WEAK int uart0_write(void* source, uint32_t length) {
	void *buffer = NULL;
	lf_return_t retval;
	struct _lf_device *device = lf_get_selected();
	lf_assert(device, fail, E_UNIMPLEMENTED, "NULL device for uart0_write.");
	lf_assert(lf_malloc(device, length, &buffer) == lf_success, fail, E_NULL, "Failed to allocate remote memory for uart0_write.");
	lf_assert(lf_push(device, buffer, source, length) == lf_success, fail, E_UNIMPLEMENTED, "Failed to push uart0.");
	lf_assert(lf_invoke(device, "uart0", _uart0_write, lf_int_t, &retval, lf_args(lf_ptr(buffer), lf_infer(length))) == lf_success, fail, E_UNIMPLEMENTED, "Failed to invoke uart0_write.");
	lf_assert(lf_free(device, buffer) == lf_success, fail, E_UNIMPLEMENTED, "Failed to free uart0_write memory.");
	return (int)retval;
fail:
	return lf_error;
}

LF_WEAK uint8_t uart0_get(void) {
	lf_return_t retval;
	lf_invoke(lf_get_selected(), "uart0", _uart0_get, lf_int8_t, &retval, NULL);
	return (uint8_t)retval;
}

LF_WEAK void uart0_put(uint8_t byte) {
	lf_return_t retval;
	lf_invoke(lf_get_selected(), "uart0", _uart0_put, lf_void_t, &retval, lf_args(lf_infer(byte)));

}

LF_WEAK int uart0_ready(void) {
	lf_return_t retval;
	lf_invoke(lf_get_selected(), "uart0", _uart0_ready, lf_int_t, &retval, NULL);
	return (int)retval;
}

LF_WEAK int uart0_reset(void) {
	lf_return_t retval;
	lf_invoke(lf_get_selected(), "uart0", _uart0_reset, lf_int_t, &retval, NULL);
	return (int)retval;
}

LF_WEAK int uart0_setbaud(uint32_t baud) {
	lf_return_t retval;
	lf_invoke(lf_get_selected(), "uart0", _uart0_setbaud, lf_int_t, &retval, lf_args(lf_infer(baud)));
	return (int)retval;
}

LF_WEAK int uart0_configure(void) {
	lf_return_t retval;
	lf_invoke(lf_get_selected(), "uart0", _uart0_configure, lf_int_t, &retval, NULL);
	return (int)retval;
}

LF_WEAK void uart0_enable(void) {
	lf_return_t retval;
	lf_invoke(lf_get_selected(), "uart0", _uart0_enable, lf_void_t, &retval, NULL);

}
