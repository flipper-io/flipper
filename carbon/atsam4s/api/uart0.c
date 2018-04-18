#include <flipper.h>

LF_FUNC("uart0") int uart0_configure(void) {
	/* Create a pinmask for the peripheral pins. */
	const unsigned int UART0_PIN_MASK = (PIO_PA9A_URXD0 | PIO_PA10A_UTXD0);
	/* Enable the peripheral clock. */
	PMC->PMC_PCER0 = (1 << ID_UART0);
	/* Disable PIOA interrupts on the peripheral pins. */
	PIOA->PIO_IDR = UART0_PIN_MASK;
	/* Disable the peripheral pins from use by the PIOA. */
	PIOA->PIO_PDR = UART0_PIN_MASK;
	/* Hand control of the peripheral pins to peripheral A. */
	PIOA->PIO_ABCDSR[0] &= ~UART0_PIN_MASK;
	PIOA->PIO_ABCDSR[1] &= ~UART0_PIN_MASK;
	/* Reset the peripheral and disable the transmitter and receiver. */
	UART0->UART_CR = UART_CR_RSTRX | UART_CR_RSTTX | UART_CR_TXDIS | UART_CR_RXDIS | UART_CR_RSTSTA;
	/* Set the mode to 8n1. */
	UART0->UART_MR = UART_MR_CHMODE_NORMAL | UART_MR_PAR_NO;
	/* Set the baudrate. */
	UART0->UART_BRGR = (F_CPU / PLATFORM_BAUDRATE / 16);
	UART0->UART_PTCR = UART_PTCR_TXTDIS | UART_PTCR_RXTDIS;
	UART0->UART_IER = UART_IER_OVRE | UART_IER_FRAME | UART_IER_PARE;
	/* Set the UART0 priority to high. */
	NVIC_SetPriority(UART0_IRQn, UART0_PRIORITY);
	/* Enable the UART0 interrupt. */
	NVIC_EnableIRQ(UART0_IRQn);
	/* Enable the transmitter and receiver. */
	UART0->UART_CR = UART_CR_TXEN | UART_CR_RXEN;
	return lf_success;
}

LF_FUNC("uart0") int uart0_setbaud(uint32_t baud) {
	return lf_success;
}

LF_FUNC("uart0") int uart0_reset(void) {
	return lf_success;
}

LF_FUNC("uart0") int uart0_ready(void) {
	/* Return the empty condition of the transmitter FIFO. */
	return (UART0->UART_SR & UART_SR_TXEMPTY);
}

LF_FUNC("uart0") void uart0_enable(void) {

}

LF_FUNC("uart0") void uart0_disable(void) {

}

LF_FUNC("uart0") void uart0_put(uint8_t byte) {
	/* Wait until ready to transmit. */
	while (!(UART0->UART_SR & UART_SR_TXEMPTY));
	/* Load the byte into the transmitter FIFO. */
	UART0->UART_THR = byte;
}

LF_FUNC("uart0") uint8_t uart0_get(void) {
	/* Retrieve a byte from the receiver FIFO. */
	return UART0->UART_RHR;
}

LF_FUNC("uart0") int uart0_write(void *src, uint32_t length) {
	UART0->UART_TCR = length;
	UART0->UART_TPR = (uintptr_t)src;
	UART0->UART_PTCR = UART_PTCR_TXTEN;
	while (!(UART0->UART_SR & UART_SR_ENDTX) || !(UART0->UART_SR & UART_SR_TXEMPTY) || !(UART0->UART_SR & UART_SR_TXRDY));
	UART0->UART_PTCR = UART_PTCR_TXTDIS;
	return lf_success;
}

LF_FUNC("uart0") int uart0_read(void *dst, uint32_t length) {
	UART0->UART_RCR = length;
	UART0->UART_RPR = (uintptr_t)dst;
	UART0->UART_PTCR = UART_PTCR_RXTEN;
#ifdef __uart0_read_sync__
	/* Wait until the transfer has finished. */
	while (!(UART0->UART_SR & UART_SR_ENDRX) || !(UART0->UART_SR & UART_SR_RXRDY));
	/* Disable the PDC receiver. */
	UART0->UART_PTCR = UART_PTCR_RXTDIS;
#endif
	return lf_success;
}
