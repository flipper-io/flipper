#ifndef __io_h__

#define __io_h__

#include <types.h>

enum { _io_configure, _io_set_direction, _io_write, _io_read };

extern const struct _io {
	
	void (* configure)(void);
	
	void (* direction)(uint8_t pin, uint8_t direction);
	
	void (* write)(uint8_t pin, uint16_t value);
	
	uint16_t (* read)(uint8_t pin);
	
} io;

#ifdef __private_include__

extern void io_configure(void);

extern void io_set_direction(uint8_t pin, uint8_t direction);

extern void io_write(uint8_t pin, uint16_t value);

extern uint16_t io_read(uint8_t pin);

#endif

#endif