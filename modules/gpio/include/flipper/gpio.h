#ifndef __gpio_h__
#define __gpio_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

/* Declare the virtual interface for this module. */
extern const struct _gpio {
	void (* configure)(void);
	/* Sets the logical direction of a GPIO pin. (0: input, 1: output) */
	void (* direction)(uint8_t pin, uint8_t direction);
	/* Writes a digital value to the specified GPIO pin. */
	void (* write)(uint8_t pin, uint16_t value);
	/* Reads a digital value from the specified GPIO pin. */
	uint16_t (* read)(uint8_t pin);
} gpio;

#ifdef __private_include__

/* Declare the FMR overlay for this driver. */
enum { _gpio_configure, _gpio_set_direction, _gpio_write, _gpio_read };

/* Declare each prototype for all functgpions within this driver. */
void gpio_configure(void);
void gpio_set_direction(uint8_t pin, uint8_t direction);
void gpio_write(uint8_t pin, uint16_t value);
uint16_t gpio_read(uint8_t pin);

#endif
#endif
