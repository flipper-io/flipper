#ifndef __gpio_h__
#define __gpio_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

/* Declare the virtual interface for this module. */
extern const struct _gpio {
	int (* configure)(void);
	/* Sets the pin mask and proprties of the PIO array. */
	void (* direction)(uint32_t mask, uint8_t properties);
	/* Writes a digital value to the specified GPIO pin. */
	void (* write)(uint32_t mask, uint8_t value);
	/* Reads a digital value from the specified GPIO pin. */
	uint16_t (* read)(uint8_t pin);
} gpio;

#ifdef __private_include__

/* Declare the FMR overlay for this driver. */
enum { _gpio_configure, _gpio_enable, _gpio_write, _gpio_read };

/* Declare each prototype for all functgpions within this driver. */
int gpio_configure(void);
void gpio_enable(uint32_t mask, uint8_t properties);
void gpio_write(uint32_t mask, uint8_t value);
uint16_t gpio_read(uint8_t pin);

#endif
#endif
