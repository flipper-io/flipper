#ifndef __spi_h__
#define __spi_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper.h>

/* Declare the prototypes for all of the functions within this module. */
int spi_configure(void);
void spi_enable(void);
void spi_disable(void);
uint8_t spi_ready(void);
void spi_put(uint8_t byte);
void spi_end(void);
uint8_t spi_get(void);
int spi_write(void *src, uint32_t length);
int spi_read(void *dst, uint32_t length);

#endif
