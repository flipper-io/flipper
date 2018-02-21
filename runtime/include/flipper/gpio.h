#ifndef __gpio_h__
#define __gpio_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper.h>

/* Declare the virtual interface for this module. */
extern const struct _gpio_interface {
	int (* configure)(void);
	/* Sets the pin mask and proprties of the PIO array. */
	void (* enable)(uint32_t enable, uint32_t disable);
	/* Writes a digital value to the specified GPIO pin. */
	void (* write)(uint32_t set, uint32_t clear);
	/* Reads a digital value from the specified GPIO pin. */
	uint32_t (* read)(uint32_t mask);
} gpio;

/* Declare the _lf_module structure for this module. */
extern struct _lf_module _gpio;

/* Declare the FMR overlay for this module. */
enum { _gpio_configure, _gpio_enable, _gpio_write, _gpio_read };

/* Declare each prototype for all functgpions within this driver. */
int gpio_configure(void);
void gpio_enable(uint32_t enable, uint32_t disable);
void gpio_write(uint32_t set, uint32_t clear);
uint32_t gpio_read(uint32_t mask);

#endif
