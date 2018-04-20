#include <flipper/i2c.h>


int i2c_configure(void) {
	/* Enable the TWI clock. */
	PMC -> PMC_PCER0 = (1 << ID_TWI0);
	/* Create a pinmask for the peripheral pins. */
	const unsigned int I2C_PIN_MASK = (PIO_PA3A_TWD0 | PIO_PA4A_TWCK0);
	/* Disable PIOA interrupts on the peripheral pins. */
	PIOA -> PIO_IDR = I2C_PIN_MASK;
	/* Disable the peripheral pins from use by the PIOA. */
	PIOA -> PIO_PDR = I2C_PIN_MASK;
	/* Hand control of the peripheral pins to peripheral A. */
	PIOA -> PIO_ABCDSR[0] &= ~I2C_PIN_MASK;
	PIOA -> PIO_ABCDSR[1] &= ~I2C_PIN_MASK;
	/* Reset the TWI. */
	TWI0 -> TWI_CR = TWI_CR_SVDIS | TWI_CR_MSDIS;
	TWI0 -> TWI_CR = TWI_CR_SWRST;
	/* Enter master mode. */
	TWI0 -> TWI_CR = TWI_CR_MSEN;
	return lf_success;
}

/* Starts a read session. */
void i2c_start_read(uint8_t address, uint8_t length) {

}

/* Contiunues reading once a read session has begun. */
uint8_t i2c_read(void) {
	return 0;
}

/* Starts a write session. */
void i2c_start_write(uint8_t address, uint8_t length) {

}

/* Continues writing once a write session has begun. */
void i2c_write(uint8_t byte) {

}

/* Stops the active session. */
void i2c_stop(void) {

}
