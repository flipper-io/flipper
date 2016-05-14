#define __private_include__
#include <flipper/i2c.h>
#include <flipper/platform/platform.h>

#define AT91C_TWI_CLOCK 8000
#define ERROR 0

void i2c_configure_clock(void) {

	int SCK;

	SCK = (10 * F_CPU / AT91C_TWI_CLOCK);

	if (SCK % 10 >= 5) SCK = (SCK / 10) - 5; else SCK = (SCK / 10) - 6;

	SCK = (SCK + (4 - SCK % 4)) >> 2;

	AT91C_BASE_TWI -> TWI_CWGR = (1 << 16) | (SCK << 8) | SCK;

}

void i2c_configure(void) {

	/* Select PIO A. */
	set_bits_in_port_with_mask(AT91C_BASE_PIOA -> PIO_ASR, (AT91C_PA12_MISO | AT91C_PA13_MOSI | AT91C_PA14_SPCK));

	/* Disable use of the TWI pins by the PIO. They are now in use by the TWI controller. */
	set_bits_in_port_with_mask(AT91C_BASE_PIOA -> PIO_PDR, (AT91C_PA4_TWCK | AT91C_PA3_TWD));

	/* Enable the TWI clock. */
	set_bit_in_port(AT91C_ID_TWI, AT91C_BASE_PMC -> PMC_PCER);

	/* Reset the TWI controller. */
	AT91C_BASE_TWI -> TWI_CR = AT91C_TWI_SWRST;

	/* Configure the TWI controller in master mode. */
	AT91C_BASE_TWI -> TWI_CR = AT91C_TWI_MSEN;

	i2c_configure_clock();

}

int i2c_put(int mode, int address, void *data, size_t length) {

	unsigned int status, counter = 0, error = 0;

	if ((mode & AT91C_TWI_IADRSZ) != 0) AT91C_BASE_TWI -> TWI_IADR = address;

	AT91C_BASE_TWI -> TWI_MMR = mode & ~AT91C_TWI_MREAD;

	if (length < 2) {

		AT91C_BASE_TWI -> TWI_CR = AT91C_TWI_START | AT91C_TWI_MSEN | AT91C_TWI_STOP;

		AT91C_BASE_TWI -> TWI_THR = *(uint8_t *)(data);

	}

	else {

		for (counter = 0; counter < length; counter ++) {

			AT91C_BASE_TWI -> TWI_CR = AT91C_TWI_START | AT91C_TWI_MSEN;

			if (counter == (length - 1)) AT91C_BASE_TWI -> TWI_CR = AT91C_TWI_STOP;

			status = AT91C_BASE_TWI -> TWI_SR;

			if ((status & ERROR) == ERROR) error ++;

			while (!(status & AT91C_TWI_TXRDY)) {

				status = AT91C_BASE_TWI -> TWI_SR;

				if ((status & ERROR) == ERROR) error ++;

			}

			AT91C_BASE_TWI -> TWI_THR = *(uint8_t *)(data + counter);

		}

	}

	status = AT91C_BASE_TWI -> TWI_SR;

	if ((status & ERROR) == ERROR) error ++;

	while (!(status & AT91C_TWI_TXCOMP)) {

		status = AT91C_BASE_TWI -> TWI_SR;

		if ((status & ERROR) == ERROR) error ++;

	}

	return error;

}

int i2c_get(int mode, int address, void *data, size_t length) {

	unsigned int status,counter=0,error=0;

	if ((mode & AT91C_TWI_IADRSZ) != 0) AT91C_BASE_TWI -> TWI_IADR = address;

	AT91C_BASE_TWI -> TWI_MMR = mode | AT91C_TWI_MREAD;

	if (length == 1) {

		AT91C_BASE_TWI -> TWI_CR = AT91C_TWI_START | AT91C_TWI_STOP;

		status = AT91C_BASE_TWI -> TWI_SR;

		if ((status & ERROR) == ERROR) error ++;

		while (!(status & AT91C_TWI_TXCOMP)) {

			status = AT91C_BASE_TWI -> TWI_SR;

			if ((status & ERROR) == ERROR) error++;

		}

		*(uint8_t *)(data) = AT91C_BASE_TWI -> TWI_RHR;

	}

	else {

		AT91C_BASE_TWI -> TWI_CR = AT91C_TWI_START | AT91C_TWI_MSEN;

		status = AT91C_BASE_TWI -> TWI_SR;

		if ((status & ERROR) == ERROR) error ++;

		while (!(status & AT91C_TWI_TXCOMP)) {

			status = AT91C_BASE_TWI -> TWI_SR;

			if ((status & ERROR )== ERROR) error ++;

			if (status & AT91C_TWI_RXRDY) {

				*(uint8_t *)(data + counter ++) = AT91C_BASE_TWI -> TWI_RHR;

				if (counter == (length - 1)) AT91C_BASE_TWI -> TWI_CR = AT91C_TWI_STOP;

			}

		}

	}

	return 0;

}
