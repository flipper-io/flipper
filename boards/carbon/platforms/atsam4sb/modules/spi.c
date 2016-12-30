#define __private_include__
#include <flipper/carbon/modules/spi.h>
#include <flipper/carbon/platforms/atsam4s16b.h>

int spi_configure() {
	/* Enable the SPI clock. */
	PMC -> PMC_PCER0 = (1 << ID_SPI);
	/* Create a pinmask for the peripheral pins. */
	const unsigned int SPI_PIN_MASK = (PIO_PA14A_SPCK | PIO_PA13A_MOSI | PIO_PA12A_MISO | PIO_PA31A_NPCS1);
	/* Disable PIOA interrupts on the peripheral pins. */
	PIOA -> PIO_IDR = SPI_PIN_MASK;
	/* Disable the peripheral pins from use by the PIOA. */
	PIOA -> PIO_PDR = SPI_PIN_MASK;
	/* Hand control of the peripheral pins to peripheral A. */
	PIOA -> PIO_ABCDSR[0] &= ~SPI_PIN_MASK;
	PIOA -> PIO_ABCDSR[1] &= ~SPI_PIN_MASK;
	/* Reset the SPI. */
	SPI -> SPI_CR = SPI_CR_SWRST;
	/* Reset the SPI again. Eratta. */
	SPI -> SPI_CR = SPI_CR_SWRST;
	/* Enable the mode fault interrupt. */
	SPI -> SPI_IER = SPI_IER_MODF;
	/* Enter master mode, no mode fault detection, activate user SPI peripheral. */
	SPI -> SPI_MR = SPI_MR_PCS(1) | SPI_MR_MSTR;
	/* Configure the user SPI peripheral. 8 bits per transfer. SPI mode 3. SCK = MCK / 8. */
	SPI -> SPI_CSR[1] = SPI_CSR_SCBR(8) | SPI_CSR_DLYBCT(1) | SPI_CSR_BITS_8_BIT | SPI_CSR_CPOL;
	/* Disable the PDC channels. */
	SPI -> SPI_PTCR = SPI_PTCR_TXTDIS | SPI_PTCR_RXTDIS;
	/* Clear the secondary PDC channel. */
	SPI -> SPI_TNCR = 0;
	SPI -> SPI_TNPR = (uintptr_t)(NULL);
	/* Enable the SPI interrupt. */
	NVIC_EnableIRQ(SPI_IRQn);
	/* Enable the SPI. */
	SPI -> SPI_CR = SPI_CR_SPIEN;
	return lf_success;
}

void spi_enable(void) {
	SPI -> SPI_CR = SPI_CR_SPIEN;
}

void spi_disable(void) {
	SPI -> SPI_CR = SPI_CR_SPIDIS;
}

uint8_t spi_ready(void) {
	return (SPI -> SPI_SR & SPI_SR_TXEMPTY);
}

void spi_put(uint8_t byte) {
	/* Transmit the byte. */
	SPI -> SPI_TDR = byte;
	/* Wait until data has been transmitted. */
	while (!(SPI -> SPI_SR & SPI_SR_TDRE));
}

uint8_t spi_get(void) {
	/* Write a dummy byte. */
	spi_put(0xff);
	/* Wait until data has been recieved. */
	while (!(SPI -> SPI_SR & SPI_SR_RDRF));
	/* Return the received byte. */
	return SPI -> SPI_RDR;
}

int spi_push(void *source, uint32_t length) {
	/* Set the transmission length and destination pointer. */
	SPI -> SPI_TCR = length;
	SPI -> SPI_TPR = (uintptr_t)(source);
	/* Enable the PDC transmitter to start the transmission. */
	SPI -> SPI_PTCR = SPI_PTCR_TXTEN;
	/* Wait until the transfer has finished. */
	while (!(SPI -> SPI_SR & SPI_SR_ENDTX));
	/* Disable the PDC transmitter. */
	SPI -> SPI_PTCR = SPI_PTCR_TXTDIS;
	return lf_success;
}

#define __spi_pull_sync__
int spi_pull(void *destination, uint32_t length) {
	/* Set the transmission length and destination pointer. */
	SPI -> SPI_RCR = length;
	SPI -> SPI_RPR = (uintptr_t)(destination);
	/* Enable the receiver. */
	SPI -> SPI_PTCR = SPI_PTCR_RXTEN;
	/* If defined, usart_pull will not use interrupts. */
#ifdef __spi_pull_sync__
	/* Wait until the transfer has finished. */
	while (!(SPI -> SPI_SR & SPI_SR_ENDRX));
	/* Disable the PDC receiver. */
	SPI -> SPI_PTCR = SPI_PTCR_RXTDIS;
#endif
	return lf_success;
}

/* Interrupt hander for this peripheral. */

void spi_isr(void) {
	/* Falls through if a mode fault has occured. This fires when the masters drive the slave out of sync. */
	if (SPI -> SPI_SR & SPI_SR_MODF) {
		/* Re-enable the SPI bus. */
		SPI -> SPI_CR = SPI_CR_SPIEN;
	}
}
