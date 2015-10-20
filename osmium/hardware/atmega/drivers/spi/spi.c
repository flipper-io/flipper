#define __private_include__

#include <spi/spi.h>

#include <platform/atmega.h>

#define SPI_DATA_MODE_0				0x00

#define SPI_DATA_MODE_1				0x04

#define SPI_DATA_MODE_2				0x08

#define SPI_DATA_MODE_3				0x0C

#define SPI_2X (1 << 0)

void spi_configure(void *configuration) {
	
	/* ~ Set MOSI and SCK as outputs. ~ */
	
	set_bits_in_port_with_mask(SPI_PORT, (bit(MOSI) | bit(SCK)));
	
	/* ~ Configure the SPI to send the most significant byte (MSB) first. ~ */
	
	clear_bit_in_port(DORD, SPCR);
	
	/* ~ Put the SPI bus into Master MODE3. ~ */
	
	set_bits_in_port_with_mask(SPCR, bit(MSTR) | SPI_DATA_MODE_3);
	
	/* ~ Configure the SPI clock to be 1/2 of the system clock. This is the fastest SCK we can specify.  ~ */
	
	set_bits_in_port_with_mask(SPSR, SPI_2X);
	
	/* ~ ~ Enable the SPI bus and enter master mode. ~ ~ */
	
	set_bit_in_port(SPE, SPCR);
	
}

void spi_enable(void) {
	
	/* ~ Enable the SPI bus by setting the SPI enable bit in the SPCR. ~ */
	
	set_bit_in_port(SPE, SPCR);
	
}

void spi_disable(void) {
	
	/* ~ Disable the SPI bus by clearing the SPI enable bit in the SPCR. ~ */
	
	clear_bit_in_port(SPE, SPCR);
	
}

bool spi_ready(void) {
	
	return 0;
	
}

void spi_put(uint8_t byte) {
	
	SPDR = byte;
	
	while (!((SPSR) & (1 << SPIF)));
	
}

uint8_t spi_get(void) {
	
	spi_put(0x00);
	
	return SPDR;
	
}

void spi_push(void *source, uint32_t length) {
	
	while (length --) spi_put(*(uint8_t *)(source ++));
	
}

void spi_pull(void *destination, uint32_t length) {
	
	while (length --) *(uint8_t *)(destination ++) = spi_get();
	
}