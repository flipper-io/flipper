#ifndef __i2c_h__
#define __i2c_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper.h>

/* Declare the prototypes for all of the functions within this module. */
int i2c_configure(void);
void i2c_start_read(uint8_t address, uint8_t length);
uint8_t i2c_read(void);
void i2c_start_write(uint8_t address, uint8_t length);
void i2c_write(uint8_t byte);
void i2c_stop(void);

#endif
