#define __private_include__
#include <flipper/spi.h>

#ifdef __use_spi__
/* Define the virtual interface for this module. */
const struct _spi spi = {
	spi_configure,
	spi_enable,
	spi_disable,
	spi_ready,
	spi_put,
	spi_get,
	spi_push,
	spi_pull,
};
#endif
