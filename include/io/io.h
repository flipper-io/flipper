#ifndef __io_h__

#define __io_h__

#include <flipper/types.h>

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
	IO016,
	
	A0,
	A1,
	A2,
	A3,
	A4,			/* ~ ANALOG IO ~ */
	A5,
	A7,
	A8,
	
};

extern const struct _io {
	
	void (* configure)(void);
	
	void (* direction)(uint8_t pin, uint8_t direction);
	
	void (* write)(uint8_t pin, uint16_t value);
	
	uint16_t (* read)(uint8_t pin);
	
} io;

#ifdef __private_include__

enum { _io_configure, _io_set_direction, _io_write, _io_read };

extern void io_configure(void);

extern void io_set_direction(uint8_t pin, uint8_t direction);

extern void io_write(uint8_t pin, uint16_t value);

extern uint16_t io_read(uint8_t pin);

#endif

#endif