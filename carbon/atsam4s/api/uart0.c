#include "libflipper.h"
#include "uart0.h"

LF_FUNC("uart0") int uart0_configure(void) {

    return lf_success;
}

LF_FUNC("uart0") int uart0_setbaud(uint32_t baud) {
    return lf_success;
}

LF_FUNC("uart0") int uart0_reset(void) {
    return lf_success;
}

LF_FUNC("uart0") int uart0_ready(void) {
    return 0;
}

LF_FUNC("uart0") void uart0_enable(void) {

}

LF_FUNC("uart0") void uart0_disable(void) {

}

LF_FUNC("uart0") void uart0_put(uint8_t byte) {

}

LF_FUNC("uart0") uint8_t uart0_get(void) {
    return 0;
}

LF_FUNC("uart0") int uart0_write(void *src, uint32_t length) {
    return lf_success;
}

LF_FUNC("uart0") int uart0_read(void *dst, uint32_t length) {
    return lf_success;
}
