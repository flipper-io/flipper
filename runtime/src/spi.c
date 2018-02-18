#include <flipper/spi.h>

#ifdef __use_spi__

LF_MODULE(_spi, "spi", "Provides control over the device's SPI bus.", NULL, NULL);

/* Define the virtual interface for this module. */
const struct _spi_interface spi = {
	spi_configure,
	spi_enable,
	spi_disable,
	spi_ready,
	spi_put,
	spi_get,
	spi_push,
	spi_pull,
};

LF_WEAK int spi_configure() {
	return lf_invoke(lf_get_current_device(), &_spi, _spi_configure, lf_int_t, NULL);
}

LF_WEAK void spi_enable(void) {
	lf_invoke(lf_get_current_device(), &_spi, _spi_enable, lf_int_t, NULL);
}

LF_WEAK void spi_disable(void) {
	lf_invoke(lf_get_current_device(), &_spi, _spi_disable, lf_int_t, NULL);
}

LF_WEAK uint8_t spi_ready(void) {
	return lf_invoke(lf_get_current_device(), &_spi, _spi_ready, lf_int_t, NULL);
}

LF_WEAK void spi_put(uint8_t byte) {
	lf_invoke(lf_get_current_device(), &_spi, _spi_put, lf_int_t, lf_args(lf_infer(byte)));
}

LF_WEAK uint8_t spi_get(void) {
	return lf_invoke(lf_get_current_device(), &_spi, _spi_get, lf_int_t, NULL);
}

LF_WEAK int spi_push(void *source, uint32_t length) {
	return lf_push(lf_get_current_device(), &_spi, _spi_push, source, length, NULL);
}

LF_WEAK int spi_pull(void *destination, uint32_t length) {
	return lf_pull(lf_get_current_device(), &_spi, _spi_pull, destination, length, NULL);
}

#endif
