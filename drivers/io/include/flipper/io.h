#ifndef __io_h__
#define __io_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

/* Decare a global enumerator to expose the IO namespace. */
enum {

	RESERVED,	/* Offset the enumerator by one so regular numbers can be used for pins IO1 through IO16. */

	IO1,
	IO2,
	IO3,
	IO4,
	IO5,
	IO6,
	IO7,
	IO8,		/* DIGITAL IO */
	IO9,
	IO10,
	IO11,
	IO12,
	IO13,
	IO14,
	IO15,
	IO16,

	A1,
	A2,
	A3,
	A4,
	A5,			/* ANALOG IO */
	A6,
	A8,

};

/* Declare the virtual interface for this module. */
extern const struct _io {

	void (* configure)(void);
	void (* direction)(uint8_t pin, uint8_t direction);
	void (* write)(uint8_t pin, uint16_t value);
	uint16_t (* read)(uint8_t pin);

} io;

#ifdef __private_include__

/* Declare the FMR overlay for this driver. */
enum { _io_configure, _io_set_direction, _io_write, _io_read };

/* Declare each prototype for all functions within this driver. */
void io_configure(void);
void io_set_direction(uint8_t pin, uint8_t direction);
void io_write(uint8_t pin, uint16_t value);
uint16_t io_read(uint8_t pin);

void analog_set_direction(uint8_t pin, uint8_t direction);
void analog_write(uint8_t pin, uint16_t value);
uint16_t analog_read(uint8_t pin);

void digital_set_direction(uint8_t pin, uint8_t direction);
void digital_write(uint8_t pin, uint16_t value);
uint8_t digital_read(uint8_t pin);
void digital_pulse(uint8_t pin, uint16_t cycle);

#endif
#endif
