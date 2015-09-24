#define __private_include__

#include <spi/spi.h>

const struct _bus spi = {
	
	spi_configure,
	
	spi_enable,
	
	spi_disable,
	
	spi_ready,
	
	spi_put,
	
	spi_get,
	
	spi_push,
	
	spi_pull
	
};