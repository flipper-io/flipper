#define __private_include__
#include <flipper/usart.h>
#include <platform/atsam4s16b.h>

#define USART0_BAUDRATE 115200

int usart_configure(void) {
	/* Create a pinmask for the peripheral pins. */
	const unsigned int USART0_PIN_MASK = (PIO_PA5A_RXD0 | PIO_PA6A_TXD0);
	/* Enable the peripheral clock. */
	PMC -> PMC_PCER0 |= (1 << ID_USART0);
	/* Disable PIOA interrupts on the peripheral pins. */
	PIOA -> PIO_IDR |= USART0_PIN_MASK;
	/* Disable the peripheral pins from use by the PIOA. */
	PIOA -> PIO_PDR |= USART0_PIN_MASK;
	/* Hand control of the peripheral pins to peripheral A. */
	PIOA -> PIO_ABCDSR[0] &= ~USART0_PIN_MASK;
	PIOA -> PIO_ABCDSR[1] &= ~USART0_PIN_MASK;
	/* Reset the peripheral and disable the transmitter and receiver. */
	USART0 -> US_CR = UART_CR_RSTRX | UART_CR_RSTTX | UART_CR_TXDIS | UART_CR_RXDIS;
	/* Set the mode to 8n1. */
	USART0 -> US_MR = US_MR_CHRL_8_BIT | US_MR_PAR_NO | US_MR_NBSTOP_1_BIT;
	/* Set the baudrate. */
	USART0 -> US_BRGR = (F_CPU / USART0_BAUDRATE / 16);
	/* Disable the secondary PDC transmitter channel. */
	USART0 -> US_TNCR = 0;
	USART0 -> US_TNPR = NULL;
	/* Disable the secondary PDC receiver channel. */
	USART0 -> US_RNCR = 0;
	USART0 -> US_RNPR = NULL;
	/* Disable the PDC transmitter and receiver. */
	USART0 -> US_PTCR = US_PTCR_TXTDIS | US_PTCR_RXTDIS;
	/* Enable the USART0 interrupt. */
	NVIC_EnableIRQ(USART0_IRQn);
	/* Enable the transmitter and receiver. */
	USART0 -> US_CR = UART_CR_TXEN | UART_CR_RXEN;
	return lf_success;
}

void usart_enable(void) {
	/* Enable the transmitter and receiver. */
	USART0 -> US_CR = UART_CR_TXEN | UART_CR_RXEN;
}

void usart_disable(void) {
	/* Disable the transmitter and receiver. */
	USART0 -> US_CR = UART_CR_TXDIS | UART_CR_RXDIS;
}

uint8_t usart_ready(void) {
	/* Return the empty condition of the transmitter FIFO. */
	return (USART0 -> US_CSR & US_CSR_TXEMPTY);
}

void usart_put(uint8_t byte) {
	/* Load the byte into the transmitter FIFO. */
	USART0 -> US_THR = byte;
}


uint8_t usart_get(void) {
	/* Retrieve a byte from the receiver FIFO. */
	return USART0 -> US_RHR;
}

int usart_push(void *source, lf_size_t length) {
	/* Set the transmission length and source pointer. */
	USART0 -> US_TCR = length;
	USART0 -> US_TPR = source;
	/* Enable the PDC transmitter. */
	USART0 -> US_PTCR = US_PTCR_TXTEN;
	/* Wait until the transfer has finished. */
	while (!(USART0 -> US_CSR & US_CSR_ENDTX));
	/* Disable the PDC transmitter. */
	USART0 -> US_PTCR = US_PTCR_TXTDIS;
	return lf_success;
}

#define __usart_pull_async__
int usart_pull(void *destination, lf_size_t length) {
	/* Set the transmission length and destination pointer. */
	USART0 -> US_RCR = length;
	USART0 -> US_RPR = destination;
	/* Enable the receiver. */
	USART0 -> US_PTCR = US_PTCR_RXTEN;
	/* If defined, usart_pull will not use interrupts. */
#ifdef __usart_pull_async__
	/* Wait until the transfer has finished. */
	while (!(USART0 -> US_CSR & US_CSR_ENDRX));
	/* Disable the PDC receiver. */
	USART0 -> US_PTCR = US_PTCR_RXTDIS;
#endif
	return lf_success;
}
