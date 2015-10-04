#ifndef power_h

#define power_h

#include <flipper/types.h>

#define eeprom_power_on_after_reset (uint8_t *) 0x0000

#define u2_power_on_after_reset(value) eeprom_update_byte(eeprom_power_on_after_reset, value);

#define power_on_scheduled() eeprom_read_byte(eeprom_power_on_after_reset)

extern bool u2_power_on;

extern void flipper_reset(void);

extern void flipper_sleep(void);

#endif