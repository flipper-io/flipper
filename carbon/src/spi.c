#include <flipper.h>

enum { _spi_read, _spi_write, _spi_get, _spi_put, _spi_end, _spi_ready, _spi_disable, _spi_enable, _spi_configure };

int spi_read(void* dst, uint32_t length);
int spi_write(void* src, uint32_t length);
uint8_t spi_get(void);
void spi_put(uint8_t byte);
void spi_end(void);
uint8_t spi_ready(void);
void spi_disable(void);
void spi_enable(void);
int spi_configure(void);

void *spi_interface[] = {
	&spi_read,
	&spi_write,
	&spi_get,
	&spi_put,
	&spi_end,
	&spi_ready,
	&spi_disable,
	&spi_enable,
	&spi_configure
};

LF_MODULE(spi, "spi", spi_interface);

LF_WEAK int spi_read(void* dst, uint32_t length) {
	lf_return_t retval;
	lf_invoke(lf_get_selected(), "spi", _spi_read, lf_int_t, &retval, lf_args(lf_infer(dst), lf_infer(length)));
	return (int)retval;
}

LF_WEAK int spi_write(void* src, uint32_t length) {
	lf_return_t retval;
	lf_invoke(lf_get_selected(), "spi", _spi_write, lf_int_t, &retval, lf_args(lf_infer(src), lf_infer(length)));
	return (int)retval;
}

LF_WEAK uint8_t spi_get(void) {
	lf_return_t retval;
	lf_invoke(lf_get_selected(), "spi", _spi_get, lf_int8_t, &retval, NULL);
	return (uint8_t)retval;
}

LF_WEAK void spi_put(uint8_t byte) {
	lf_return_t retval;
	lf_invoke(lf_get_selected(), "spi", _spi_put, lf_void_t, &retval, lf_args(lf_infer(byte)));
	
}

LF_WEAK void spi_end(void) {
	lf_return_t retval;
	lf_invoke(lf_get_selected(), "spi", _spi_end, lf_void_t, &retval, NULL);
	
}

LF_WEAK uint8_t spi_ready(void) {
	lf_return_t retval;
	lf_invoke(lf_get_selected(), "spi", _spi_ready, lf_int8_t, &retval, NULL);
	return (uint8_t)retval;
}

LF_WEAK void spi_disable(void) {
	lf_return_t retval;
	lf_invoke(lf_get_selected(), "spi", _spi_disable, lf_void_t, &retval, NULL);
	
}

LF_WEAK void spi_enable(void) {
	lf_return_t retval;
	lf_invoke(lf_get_selected(), "spi", _spi_enable, lf_void_t, &retval, NULL);
	
}

LF_WEAK int spi_configure(void) {
	lf_return_t retval;
	lf_invoke(lf_get_selected(), "spi", _spi_configure, lf_int_t, &retval, NULL);
	return (int)retval;
}

