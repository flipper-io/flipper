#define __private_include__
#include <flipper/config.h>
#include <flipper/platform/platform.h>

void config_configure(void) {

}

void config_write(uint8_t key, uint16_t value) {

	/* ~ Wait until the EEPROM is ready. ~ */
	eeprom_busy_wait();

	/* ~ Write the configuration into EEPROM. ~ */
	eeprom_update_word((uint16_t *)(key * sizeof(uint16_t)), value);

}

uint16_t config_read(uint8_t key) {

	/* ~ Wait until the EEPROM is ready. ~ */
	eeprom_busy_wait();

	/* ~ Read the configuration from EEPROM. ~ */
	return eeprom_read_word((uint16_t *)(key * sizeof(uint16_t)));

}