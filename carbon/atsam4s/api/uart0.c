#include <flipper.h>
#include "uart/uart.h"
#include "pdc/pdc.h"

LF_FUNC("uart0") int uart0_configure(void) {

    sam_uart_opt_t opt;

    uart_init(UART0, &opt);
    uart_enable_interrupt(UART0, 0);

	return lf_success;
}

LF_FUNC("uart0") int uart0_setbaud(uint32_t baud) {
    sam_uart_opt_t opt;
    opt.ul_baudrate = baud;
    uart_init(UART0, &opt);
	return lf_success;
}

LF_FUNC("uart0") int uart0_reset(void) {
    uart_reset(UART0);
	return lf_success;
}

LF_FUNC("uart0") int uart0_ready(void) {
	return uart_is_rx_ready(UART0);
}

LF_FUNC("uart0") void uart0_enable(void) {
    uart_enable_tx(UART0);
    uart_enable_rx(UART0);
}

LF_FUNC("uart0") void uart0_disable(void) {
    uart_disable_tx(UART0);
    uart_disable_rx(UART0);
}

LF_FUNC("uart0") void uart0_put(const uint8_t byte) {
    uart_write(UART0, byte);
}

LF_FUNC("uart0") uint8_t uart0_get(void) {
    uint8_t c;
	uart_read(UART0, &c);
    return c;
}

LF_FUNC("uart0") int uart0_write(void *src, uint32_t length) {
    pdc_packet_t p;
    p.ul_addr = (uintptr_t)src;
    p.ul_size = length;
    pdc_tx_init(PDC_UART0, &p, NULL);
    pdc_enable_transfer(PDC_UART0, 0);
	return lf_success;
}

LF_FUNC("uart0") int uart0_read(void *dst, uint32_t length) {
    pdc_packet_t p;
    p.ul_addr = (uintptr_t)dst;
    p.ul_size = length;
    pdc_rx_init(PDC_UART0, &p, NULL);
    pdc_enable_transfer(PDC_UART0, 0);
	return lf_success;
}
