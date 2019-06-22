#include "libflipper.h"

enum {
    _uart0_read,
    _uart0_write,
    _uart0_get,
    _uart0_put,
    _uart0_ready,
    _uart0_reset,
    _uart0_setbaud,
    _uart0_configure,
    _uart0_enable
};

int uart0_read(void *dst, uint32_t length);
int uart0_write(void *src, uint32_t length);
uint8_t uart0_get(void);
void uart0_put(uint8_t byte);
int uart0_ready(void);
int uart0_reset(void);
int uart0_setbaud(uint32_t baud);
int uart0_configure(void);
void uart0_enable(void);

void *uart0_interface[] = { &uart0_read,  &uart0_write,   &uart0_get,       &uart0_put,   &uart0_ready,
                            &uart0_reset, &uart0_setbaud, &uart0_configure, &uart0_enable };

LF_MODULE(uart0);

LF_WEAK int uart0_read(void *destination, uint32_t length) {
    lf_return_t retval;
    lf_invoke(lf_get_selected(), "uart0", _uart0_read, lf_int_t, &retval, lf_args(lf_ptr(destination), lf_infer(length)));
    return (uint8_t)retval;
}

LF_WEAK int uart0_write(void *source, uint32_t length) {
    lf_return_t retval;
    lf_invoke(lf_get_selected(), "uart0", _uart0_write, lf_int_t, &retval, lf_args(lf_ptr(source), lf_infer(length)));
    return (uint8_t)retval;
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
