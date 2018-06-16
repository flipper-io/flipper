#include <api/usart.h>
//#include "usart.h"

LF_FUNC("usart") int usart_configure(void) {
	return lf_success;
}

LF_FUNC("usart") void usart_enable(void) {

}

LF_FUNC("usart") void usart_disable(void) {

}

LF_FUNC("usart") int usart_ready(void) {
    return 0;
}

LF_FUNC("usart") void usart_put(uint8_t byte) {

}

LF_FUNC("usart") uint8_t usart_get(void) {
    return 0;
}

LF_FUNC("usart") int usart_write(void *src, uint32_t length) {
	return lf_success;
}

LF_FUNC("usart") int usart_read(void *dst, uint32_t length) {
	return lf_success;
}

/* uart isr */
void usart0_isr(void) {

}
