#ifndef __gpio_h__
#define __gpio_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper.h>

/* Declare each prototype for all functgpions within this driver. */
int gpio_configure(void);
void gpio_enable(uint32_t enable, uint32_t disable);
void gpio_write(uint32_t set, uint32_t clear);
uint32_t gpio_read(uint32_t mask);

#endif
