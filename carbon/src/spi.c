#include <flipper.h>

enum { __spi_read, __spi_write, _spi_get, _spi_put, _spi_end, _spi_ready, _spi_disable, _spi_enable, _spi_configure };

int _spi_read(void* destination, uint32_t length);
int _spi_write(void* source, uint32_t length);
uint8_t spi_get(void);
void spi_put(uint8_t byte);
void spi_end(void);
uint8_t spi_ready(void);
void spi_disable(void);
void spi_enable(void);
int spi_configure(void);

void *spi_interface[] = {
	&_spi_read,
	&_spi_write,
	&spi_get,
	&spi_put,
	&spi_end,
	&spi_ready,
	&spi_disable,
	&spi_enable,
	&spi_configure
};

LF_MODULE(spi, "spi", spi_interface);

LF_WEAK int _spi_read(void* destination, uint32_t length) {
	return lf_success;
}

LF_WEAK int _spi_write(void* source, uint32_t length) {
	return lf_success;
}

LF_WEAK uint8_t spi_get(void) {
	return lf_invoke(lf_get_current_device(), "spi", _spi_get, lf_int8_t, NULL);
}

LF_WEAK void spi_put(uint8_t byte) {
	lf_invoke(lf_get_current_device(), "spi", _spi_put, lf_void_t, lf_args(lf_infer(byte)));
}

LF_WEAK void spi_end(void) {
	lf_invoke(lf_get_current_device(), "spi", _spi_end, lf_void_t, NULL);
}

LF_WEAK uint8_t spi_ready(void) {
	return lf_invoke(lf_get_current_device(), "spi", _spi_ready, lf_int8_t, NULL);
}

LF_WEAK void spi_disable(void) {
	lf_invoke(lf_get_current_device(), "spi", _spi_disable, lf_void_t, NULL);
}

LF_WEAK void spi_enable(void) {
	lf_invoke(lf_get_current_device(), "spi", _spi_enable, lf_void_t, NULL);
}

LF_WEAK int spi_configure(void) {
	return lf_invoke(lf_get_current_device(), "spi", _spi_configure, lf_int_t, NULL);
}

