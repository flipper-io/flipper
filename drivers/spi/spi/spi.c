#define __private_include__
#include <flipper/spi.h>
#include <flipper/fmr.h>

void spi_configure() {

}

void spi_enable(void) {

	device.invoke(_spi, _spi_enable, NO_ARGS);

}

void spi_disable(void) {

	device.invoke(_spi, _spi_disable, NO_ARGS);

}

uint8_t spi_ready(void) {

	return (uint8_t)(device.invoke(_spi, _spi_enable, NO_ARGS));

}

void spi_put(uint8_t byte) {

	device.invoke(_spi, _spi_put, 1, byte);

}

uint8_t spi_get(void) {

	return (uint8_t)(device.invoke(_spi, _spi_put, NO_ARGS));

}

void spi_push(void *source, size_t length) {

	device.push(_spi, _spi_push, NO_ARGS, source, length);

}

void spi_pull(void *destination, size_t length) {

	device.push(_spi, _spi_pull, NO_ARGS, destination, length);

}
