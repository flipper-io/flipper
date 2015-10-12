#define __private_include__

#include <spi/spi.h>

#include "platform/at91sam.h"

void spi_configure(void *configuration) {
	
	/* ~ Select PIO A. ~ */
	
	set_bits_in_port_with_mask(AT91C_BASE_PIOA -> PIO_ASR, (AT91C_PA12_MISO | AT91C_PA13_MOSI | AT91C_PA14_SPCK));
	
	/* ~ Disable use of the SPI pins by the PIO. They are now in use by the SPI controller. ~ */
	
	set_bits_in_port_with_mask(AT91C_BASE_PIOA -> PIO_PDR, (AT91C_PA12_MISO | AT91C_PA13_MOSI | AT91C_PA14_SPCK));
	
	/* ~ Enable the SPI in the PCER (peripheral clock enable register). ~ */
	
	set_bit_in_port(AT91C_ID_SPI, AT91C_BASE_PMC -> PMC_PCER);
	
	/* ~ Disable communications and reset the SPI. ~ */
	
	set_bits_in_port_with_mask(AT91C_BASE_SPI -> SPI_CR, AT91C_SPI_SPIDIS);
	
	/* ~ Execcute another software reset of the SPI in order to ensure that the SPI controller has been completely reset then enable it. ~ */
	
	set_bits_in_port_with_mask(AT91C_BASE_SPI -> SPI_CR, AT91C_SPI_SWRST);
	
	set_bits_in_port_with_mask(AT91C_BASE_SPI -> SPI_CR, AT91C_SPI_SWRST);
	
	/* ~ Disable the PDMAC for now. ~ */
	
	AT91C_BASE_SPI -> SPI_PTCR = (AT91C_PDC_RXTDIS | AT91C_PDC_TXTDIS);
	
	/* ~ Configure the SPI as a master: fixed peripheral selection: no fault detection: deselect all peripherals.  ~ */
	
	set_bits_in_port_with_mask(AT91C_BASE_SPI -> SPI_MR, (AT91C_SPI_MSTR | AT91C_SPI_PS_FIXED | AT91C_SPI_CSAAT | (AT91C_SPI_PCS & (1 << 16)) | AT91C_SPI_MODFDIS));
	
	/* ~ SPI_DATA_MODE_3: don't change CS after transfer: 8 bits per transfer: ~ */
	
	AT91C_SPI_CSR[1] = (AT91C_SPI_CPOL | AT91C_SPI_BITS_8 | (AT91C_SPI_SCBR & (8 << 8) | (AT91C_SPI_DLYBCT & (2 << 24))));
	
	/* ~ Enable the SPI controller. ~ */
	
	set_bits_in_port_with_mask(AT91C_BASE_SPI -> SPI_CR, AT91C_SPI_SPIEN);
	
}

void spi_enable(void){
	
	set_bits_in_port_with_mask(AT91C_BASE_SPI -> SPI_CR, AT91C_SPI_SPIEN);
	
}

void spi_disable(void) {
	
	set_bits_in_port_with_mask(AT91C_BASE_SPI -> SPI_CR, AT91C_SPI_SPIDIS);
	
}

bool spi_ready(void) {
	
	return ((AT91C_BASE_SPI -> SPI_SR) & AT91C_SPI_RDRF);
	
}

void spi_put(uint8_t byte) {
	
	/* ~ Send the byte to the peripheral connected to NPCS1. ~ */
	
	AT91C_BASE_SPI -> SPI_TDR = byte;
	
	/* ~ Wait until the byte has been sent. ~ */
	
	while (!((AT91C_BASE_SPI -> SPI_SR) & AT91C_SPI_TDRE));
	
}

uint8_t spi_get(void) {
	
	/* ~ Send a don't care byte. ~ */
	
	spi_put(0x00);
	
	/* ~ Wait until data has been received. ~ */
	
	while (!((AT91C_BASE_SPI -> SPI_SR) & AT91C_SPI_RDRF));
	
	/* ~ Return the received data. ~ */
	
	return (uint8_t)(AT91C_BASE_SPI -> SPI_RDR);
	
}

void spi_push(void *source, uint32_t length) {
	
	/* ~ THIS SHOULD BE OPTIMIZED TO USE THE DMAC. ~ */
	
	while (length --) spi_put(*(uint8_t *)(source ++));
	
}

void spi_pull(void *destination, uint32_t length) {
	
	/* ~ THIS SHOULD BE OPTIMIZED TO USE THE DMAC. ~ */
	
	while (length --) *(uint8_t *)(destination ++) = spi_get();
	
}