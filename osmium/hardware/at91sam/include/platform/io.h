#ifndef __analog_h__

#define __analog_h__

#include <flipper/types.h>

#ifdef __private_include__

extern void analog_set_direction(uint8_t pin, uint8_t direction);

extern void analog_write(uint8_t pin, uint16_t value);

uint16_t analog_read(uint8_t pin);

extern void digital_set_direction(uint8_t pin, uint8_t direction);

extern void digital_write(uint8_t pin, uint16_t value);

bool digital_read(uint8_t pin);

extern void digital_pulse(uint8_t pin, uint16_t cycle);

#endif

#endif