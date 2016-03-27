#ifndef __io_h__
#define __io_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/flipper/core.h>

/* ~ Decare a global enumerator to expose the IO namespace. ~ */
enum {

	RESERVED,	/* ~ Offset the enumerator by one so regular numbers can be used for pins IO1 through IO16. ~ */

	IO1,
	IO2,
	IO3,
	IO4,
	IO5,
	IO6,
	IO7,
	IO8,		/* ~ DIGITAL IO ~ */
	IO9,
	IO10,
	IO11,
	IO12,
	IO13,
	IO14,
	IO15,
	IO16,

	A0,
	A1,
	A2,
	A3,
	A4,			/* ~ ANALOG IO ~ */
	A5,
	A7,
	A8,

};

/* ~ Declare the virtual driver object. ~ */
extern const struct _io {

	void (* configure)(void);
	void (* direction)(uint8_t pin, uint8_t direction);
	void (* write)(uint8_t pin, uint16_t value);
	uint16_t (* read)(uint8_t pin);

} io;

#ifdef __private_include__

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _io_configure, _io_set_direction, _io_write, _io_read };

/* ~ Declare all function prototypes for this driver. ~ */
extern void io_configure(void);
extern void io_set_direction(uint8_t pin, uint8_t direction);
extern void io_write(uint8_t pin, uint16_t value);
extern uint16_t io_read(uint8_t pin);

extern void analog_set_direction(uint8_t pin, uint8_t direction);
extern void analog_write(uint8_t pin, uint16_t value);
uint16_t analog_read(uint8_t pin);

extern void digital_set_direction(uint8_t pin, uint8_t direction);
extern void digital_write(uint8_t pin, uint16_t value);
bool digital_read(uint8_t pin);
extern void digital_pulse(uint8_t pin, uint16_t cycle);

#endif
#endif
