#include <flipper.h>

enum { _uart0_read, _uart0_write, _uart0_get, _uart0_put, _uart0_ready, _uart0_reset, _uart0_setbaud, _uart0_configure, _uart0_enable };

int uart0_read(void* destination, uint32_t length);
int uart0_write(void* source, uint32_t length);
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

LF_WEAK int uart0_read(void* destination, uint32_t length) {
	void *buffer = NULL;
	struct _lf_device *device = lf_get_current_device();
	lf_assert(device, failure, E_UNIMPLEMENTED, "NULL device for uart0_read.");
	lf_assert(lf_malloc(device, length, &buffer) == lf_success, failure, E_NULL, "Failed to allocate remote memory for uart0_read.");
	lf_assert(lf_invoke(device, "uart0", _uart0_read, lf_int_t, lf_args(lf_ptr(buffer), lf_infer(length))) == lf_success, failure, E_UNIMPLEMENTED, "Failed to invoke uart0 read.");
	lf_assert(lf_pull(device, destination, buffer, length) == lf_success, failure, E_UNIMPLEMENTED, "Failed to pull uart0.");
	lf_assert(lf_free(device, buffer) == lf_success, failure, E_UNIMPLEMENTED, "Failed to free uart0_read memory.");
	return lf_success;
failure:
	return lf_error;
}

LF_WEAK int uart0_write(void* source, uint32_t length) {
	void *buffer = NULL;
	struct _lf_device *device = lf_get_current_device();
	lf_assert(device, failure, E_UNIMPLEMENTED, "NULL device for uart0_read.");
	lf_assert(lf_malloc(device, length, &buffer) == lf_success, failure, E_NULL, "Failed to allocate remote memory for uart0_write.");
	lf_assert(lf_push(device, buffer, source, length) == lf_success, failure, E_UNIMPLEMENTED, "Failed to push uart0.");
	lf_assert(lf_invoke(device, "uart0", _uart0_write, lf_int_t, lf_args(lf_ptr(buffer), lf_infer(length))) == lf_success, failure, E_UNIMPLEMENTED, "Failed to invoke uart0 write.");
	lf_assert(lf_free(device, buffer) == lf_success, failure, E_UNIMPLEMENTED, "Failed to free uart0_write memory.");
	return lf_success;
failure:
	return lf_error;
}

LF_WEAK uint8_t uart0_get(void) {
	return lf_invoke(lf_get_current_device(), "uart0", _uart0_get, lf_int8_t, NULL);
}

LF_WEAK void uart0_put(uint8_t byte) {
	lf_invoke(lf_get_current_device(), "uart0", _uart0_put, lf_void_t, lf_args(lf_infer(byte)));
}

LF_WEAK int uart0_ready(void) {
	return lf_invoke(lf_get_current_device(), "uart0", _uart0_ready, lf_int_t, NULL);
}

LF_WEAK int uart0_reset(void) {
	return lf_invoke(lf_get_current_device(), "uart0", _uart0_reset, lf_int_t, NULL);
}

LF_WEAK int uart0_setbaud(uint32_t baud) {
	return lf_invoke(lf_get_current_device(), "uart0", _uart0_setbaud, lf_int_t, lf_args(lf_infer(baud)));
}

LF_WEAK int uart0_configure(void) {
	return lf_invoke(lf_get_current_device(), "uart0", _uart0_configure, lf_int_t, NULL);
}

LF_WEAK void uart0_enable(void) {
	lf_invoke(lf_get_current_device(), "uart0", _uart0_enable, lf_void_t, NULL);
}
