#include <flipper/uart0.h>

#ifdef __use_uart0__

LF_MODULE(_uart0, "uart0", "Provides low level access to the device's UART bus.", NULL, NULL);

/* Define the virtual interface for this endpoint. */
const struct _uart0_interface uart0 = {
	uart0_configure,
	uart0_ready,
	uart0_push,
	uart0_pull
};

LF_WEAK int uart0_configure(uint8_t baud, uint8_t interrupts) {
	lf_invoke(lf_get_current_device(), &_uart0, _uart0_configure, lf_int_t, lf_args(lf_infer(baud), lf_infer(interrupts)));
	return lf_success;
}

LF_WEAK int uart0_ready(void) {
	return lf_invoke(lf_get_current_device(), &_uart0, _uart0_ready, lf_int_t, NULL);
}

LF_WEAK int uart0_push(void *source, lf_size_t length) {
	return lf_push(lf_get_current_device(), &_uart0, _uart0_push, source, length, NULL);
}

LF_WEAK int uart0_pull(void *destination, lf_size_t length) {
	return lf_pull(lf_get_current_device(), &_uart0, _uart0_pull, destination, length, NULL);
}

#endif
