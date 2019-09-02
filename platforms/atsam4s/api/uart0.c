#include "libflipper.h"
#include <uart.h>

LF_FUNC int uart0_configure(void) {

    return lf_success;
}

LF_FUNC int uart0_setbaud(uint32_t baud) {
    return lf_success;
}

LF_FUNC int uart0_reset(void) {
    return lf_success;
}

LF_FUNC int uart0_ready(void) {
    return 0;
}

LF_FUNC void uart0_enable(void) {
}

LF_FUNC void uart0_disable(void) {
}

LF_FUNC void uart0_put(uint8_t byte) {
}

LF_FUNC uint8_t uart0_get(void) {
    return 0;
}

LF_FUNC int uart0_write(void *src, uint32_t length) {
    return lf_success;
}

LF_FUNC int uart0_read(void *dst, uint32_t length) {
    return lf_success;
}
