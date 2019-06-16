#include "libflipper.h"
#include <usart.h>

LF_FUNC int usart_configure(void) {
    return lf_success;
}

LF_FUNC void usart_enable(void) {
}

LF_FUNC void usart_disable(void) {
}

LF_FUNC int usart_ready(void) {
    return 0;
}

LF_FUNC void usart_put(uint8_t byte) {
}

LF_FUNC uint8_t usart_get(void) {
    return 0;
}

/*

LF_FUNC int usart_write(void *src, uint32_t length) {
    return lf_success;
}

LF_FUNC int usart_read(void *dst, uint32_t length) {
    return lf_success;
}

*/

/* uart isr */
void usart0_isr(void) {
}
