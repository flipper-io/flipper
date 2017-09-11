#define __private_include__
#include <flipper/nvm.h>

int nvm_configure(void) {
	/* Create a pinmask for the NVM pins. */
	const unsigned int NVM_PIN_MASK = (PIO_PA11A_NPCS0);
	/* Configure the NVM SPI peripheral. 8 bits per transfer. SPI mode 3. SCK = MCK. */
	SPI -> SPI_CSR[0] = SPI_CSR_SCBR(1) | SPI_CSR_DLYBCT(1) | SPI_CSR_BITS_8_BIT | SPI_CSR_NCPHA | SPI_CSR_CPOL;
	/* Disable PIOA interrupts on the peripheral pins. */
	PIOA -> PIO_IDR = NVM_PIN_MASK;
	/* Disable the peripheral pins from use by the PIOA. */
	PIOA -> PIO_PDR = NVM_PIN_MASK;
	/* Hand control of the peripheral pins to peripheral A. */
	PIOA -> PIO_ABCDSR[0] &= ~NVM_PIN_MASK;
	PIOA -> PIO_ABCDSR[1] &= ~NVM_PIN_MASK;
	return lf_success;
}

void nvm_enable(void) {

}

void nvm_disable(void) {

}

void nvm_reset(void) {

}

/* The symbols 'nvm_write', 'nvm_put', 'nvm_read', and 'nvm_get' are declared in 'osmium/src/nvm/nvm.c'. */

/* The symbols 'nvm_alloc' and 'nvm_free' are declared in alloc.c. */

void nvm_format(void) {

}
