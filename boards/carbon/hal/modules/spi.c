#define __private_include__
#include <flipper/libflipper.h>
#include <flipper/carbon/modules/spi.h>

int spi_configure() {
	return lf_invoke(&_spi, _spi_configure, NULL);
}

void spi_enable(void) {
	lf_invoke(&_spi, _spi_enable, NULL);
}

void spi_disable(void) {
	lf_invoke(&_spi, _spi_disable, NULL);
}

uint8_t spi_ready(void) {
	return lf_invoke(&_spi, _spi_ready, NULL);
}

void spi_put(uint8_t byte) {
	lf_invoke(&_spi, _spi_put, fmr_args(fmr_infer(byte)));
}

uint8_t spi_get(void) {
	return lf_invoke(&_spi, _spi_get, NULL);
}

int spi_push(void *source, uint32_t length) {
	return lf_push(&_spi, _spi_push, source, length, NULL);
}

int spi_pull(void *destination, uint32_t length) {
	return lf_pull(&_spi, _spi_pull, destination, length, NULL);
}
