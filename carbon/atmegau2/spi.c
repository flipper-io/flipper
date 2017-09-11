#define __private_include__
#include <flipper/spi.h>
#include <flipper/atmegau2/atmegau2.h>

#define SPI_DATA_MODE_0 0x00
#define SPI_DATA_MODE_1 0x04
#define SPI_DATA_MODE_2 0x08
#define SPI_DATA_MODE_3 0x0C

int spi_configure() {
	/* Configure MOSI and SCK as inputs. */
	clear_bits_in_port_with_mask(SPI_DDR, (bit(MOSI) | bit(SCK)));
	/* Put the SPI bus into MODE3. */
	set_bits_in_port_with_mask(SPCR, SPI_DATA_MODE_3);
	/* Configure the SPI clock to be 1/2 of the system clock. This is the fastest SCK we can specify.  */
	set_bits_in_port_with_mask(SPSR, bit(SPI2X));
	return lf_success;
}

void spi_enable(void) {
	/* Configure MOSI and SCK as outputs. */
	set_bits_in_port_with_mask(SPI_DDR, (bit(MOSI) | bit(SCK)));
	/* Put the SPI into master mode by setting the master bit. */
	set_bit_in_port(MSTR, SPCR);
	/* Enable the SPI bus by setting the SPI enable bit in the SPCR. */
	set_bit_in_port(SPE, SPCR);
}

void spi_disable(void) {
	/* Disable the SPI bus by clearing the SPI enable bit in the SPCR. */
	clear_bit_in_port(SPE, SPCR);
	/* Enter slave mode. */
	clear_bit_in_port(MSTR, SPCR);
	/* Configure MOSI and SCK as inputs. */
	clear_bits_in_port_with_mask(SPI_DDR, (bit(MOSI) | bit(SCK)));
}

uint8_t spi_ready(void) {
	/* The ready state of the SPI can be determined by reading its interrupt flag. */
	return (SPSR) & (1 << SPIF);
}

void spi_put(uint8_t byte) {
	/* Write the byte to the data register. */
	SPDR = byte;
	/* Wait until the byte has been sent. */
	while (!((SPSR) & (1 << SPIF)));
}

uint8_t spi_get(void) {
	/* Send a byte to begin transmission. */
	spi_put(0x00);
	/* Read the data received. */
	return SPDR;
}

int spi_push(void *source, lf_size_t length) {
	while (length --) spi_put(*(uint8_t *)(source ++));
	return lf_success;
}

int spi_pull(void *destination, lf_size_t length) {
	while (length --) *(uint8_t *)(destination ++) = spi_get();
	return lf_success;
}
