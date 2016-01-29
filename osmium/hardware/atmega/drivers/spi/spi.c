#define __private_include__

#include <spi/spi.h>

#include <platform/atmega.h>

#define SPI_DATA_MODE_0				0x00

#define SPI_DATA_MODE_1				0x04

#define SPI_DATA_MODE_2				0x08

#define SPI_DATA_MODE_3				0x0C

void spi_configure(void *configuration) {
	
	/* ~ Configure MOSI and SCK as inputs. ~ */
	
	clear_bits_in_port_with_mask(PORTB, (bit(MOSI) | bit(SCK)));
	
	clear_bits_in_port_with_mask(SPI_DDR, (bit(MOSI) | bit(SCK)));
	
	/* ~ Put the SPI bus into MODE3. ~ */
	
	set_bits_in_port_with_mask(SPCR, SPI_DATA_MODE_3);
	
	/* ~ Configure the SPI clock to be 1/2 of the system clock. This is the fastest SCK we can specify.  ~ */
	
	set_bits_in_port_with_mask(SPSR, bit(SPI2X));
	
}

void spi_enable(void) {
    
    /* ~ Configure MOSI and SCK as outputs. ~ */
    
    set_bits_in_port_with_mask(SPI_DDR, (bit(MOSI) | bit(SCK)));
	
    /* ~ Put the SPI into master mode by setting the master bit. ~ */
    
    set_bit_in_port(MSTR, SPCR);
    
    /* ~ Enable the SPI bus by setting the SPI enable bit in the SPCR. ~ */
    
    set_bit_in_port(SPE, SPCR);
	
}

void spi_disable(void) {
    
    /* ~ Disable the SPI bus by setting the SPI enable bit in the SPCR. ~ */
    
    clear_bit_in_port(SPE, SPCR);
	
	/* ~ Configure MOSI and SCK as inputs. ~ */
	
	clear_bits_in_port_with_mask(PORTB, (bit(MOSI) | bit(SCK)));
	
	clear_bits_in_port_with_mask(SPI_DDR, (bit(MOSI) | bit(SCK)));
	
}

bool spi_ready(void) {
	
    /* ~ The ready state of the SPI can be determined by reading its interrupt flag. ~ */
    
	return (SPSR) & (1 << SPIF);
	
}

void spi_put(uint8_t byte) {
	
    /* ~ Write the byte to the data register. ~ */
    
	SPDR = byte;
    
    /* ~ Wait until the byte has been sent. ~ */
	
	while (!((SPSR) & (1 << SPIF)));
	
}

uint8_t spi_get(void) {
	
    /* ~ Send a byte to begin transmission. ~ */
    
	spi_put(0x00);
    
    /* ~ Read the data received. ~ */
	
	return SPDR;
	
}

void spi_push(void *source, uint32_t length) {
	
	while (length --) spi_put(*(uint8_t *)(source ++));
	
}

void spi_pull(void *destination, uint32_t length) {
	
	while (length --) *(uint8_t *)(destination ++) = spi_get();
	
}