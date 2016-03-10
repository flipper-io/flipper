#ifndef __spi_h__
#define __spi_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* ~ Declare the virtual driver object. ~ */
extern const struct _bus spi;

#ifdef __private_include__

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _spi_configure, _spi_enable, _spi_disable, _spi_ready, _spi_put, _spi_get, _spi_push, _spi_pull };

/* ~ Declare all function prototypes for this driver. ~ */
extern void spi_configure(void);
extern void spi_enable(void);
extern void spi_disable(void);
extern bool spi_ready(void);
extern void spi_put(uint8_t byte);
extern uint8_t spi_get(void);
extern void spi_push(void *source, uint32_t length);
extern void spi_pull(void *source, uint32_t length);

#endif
#endif
