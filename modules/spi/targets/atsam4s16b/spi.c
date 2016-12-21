#define __private_include__
#include <flipper/spi.h>
#include <platform/atsam4s16b.h>

int spi_configure() {
	// /* Enable the SPI clock in the PMC. */
	// PMC_EnablePeripheral(ID_SPI);
	// /* Declare a pin map that will configure the appropriate output pins for the SPI peripheral. */
	// const Pin spi_pins[] = { (Pin){ PIO_PA14A_SPCK | PIO_PA13A_MOSI | PIO_PA12A_MISO | PIO_PA11A_NPCS0, PIOA, ID_PIOA, PIO_PERIPH_A, PIO_DEFAULT } };
	// /* Write the pinmap into the PIO. */
	// PIO_Configure(spi_pins, PIO_LISTSIZE(spi_pins));
	// /* Configure the SPI peripheral. */
	// SPI_Configure(SPI, ID_SPI, SPI_MR_MSTR | SPI_MR_MODFDIS | SPI_PCS(0));
	// /* Configure the NVM peripheral. */
	// SPI_ConfigureNPCS(SPI, 0, SPI_CSR_NCPHA | SPI_CSR_DLYBS(0) | SPI_CSR_DLYBCT(0) | SPI_CSR_SCBR(0xC8));
	// /* Enable the SPI peripheral. */
	// SPI_Enable(SPI);
	return lf_success;
}

void spi_enable(void) {
	// /* Enable the SPI peripheral. */
	// SPI_Enable(SPI);
}

void spi_disable(void) {
	// /* Disable the SPI peripheral. */
	// SPI_Disable(SPI);
}

uint8_t spi_ready(void) {
	return 0; // SPI_IsFinished(SPI);
}

void spi_put(uint8_t byte) {
	// SPI_Write(SPI, 0, byte);
}

uint8_t spi_get(void) {
	return 0; // SPI_Read(SPI);
}

void spi_push(void *source, uint32_t length) {
	// SPI_WriteBuffer(SPI, source, length);
}

void spi_pull(void *destination, uint32_t length) {
	// SPI_ReadBuffer(SPI, destination, length);
}
