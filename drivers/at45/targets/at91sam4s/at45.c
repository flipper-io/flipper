#define __private_include__
#include <flipper/at45.h>
#include <flipper/spi.h>
#include <flipper/platform/platform.h>

void at45_configure(void) {

	/* ~ Enable the CS pin. ~ */
	set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_PER);

	/* ~ Configure the CS pin as an input. ~ */
	clear_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_OER);
	set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_ODR);

}

void at45_enable(void) {

//	spi_enable();

	/* ~ Wait until SPI is ready. ~ */
	while (!((AT91C_BASE_SPI -> SPI_SR) & AT91C_SPI_SPIENS));

	/* ~ Configure the CS pin as an output. ~ */
	clear_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_ODR);
	set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_OER);

	/* ~ Pull the CS pin low to enable the device. ~ */
	clear_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_SODR);
	set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_CODR);

}

void at45_disable(void) {

	/* ~ Pull the CS pin high to disable the device. ~ */
	clear_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_CODR);
	set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_SODR);

	/* ~ Configure the CS pin as an input. ~ */
	clear_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_OER);
	set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_ODR);

//	spi_disable();

}

void at45_reset(void) {

	/* ~ Wait until SPI is ready. ~ */
	while (!((AT91C_BASE_SPI -> SPI_SR) & AT91C_SPI_SPIENS));

	/* ~ Configure the CS pin as an output. ~ */
	clear_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_ODR);
	set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_OER);

	/* ~ Pull the CS pin high to disable the device. ~ */
	clear_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_CODR);
	set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_SODR);

	/* ~ Pull the CS pin low to enable the device. ~ */
	clear_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_SODR);
	set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_CODR);

}

/*

 Note that at45_alloc() and at45_free() are defined in the common 'alloc.c' for organizational purposes.

*/

void at45_format(void) {

}
