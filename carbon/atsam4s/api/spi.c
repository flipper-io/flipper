#include <flipper/spi.h>
#include "spi/spi.h"

LF_FUNC("spi") int spi_configure() {

	return lf_success;
}

LF_FUNC("spi") void spi_enable(void) {

}

LF_FUNC("spi") void spi_disable(void) {

}

LF_FUNC("spi") uint8_t spi_ready(void) {
    return 0;
}

LF_FUNC("spi") void spi_end(void) {

}

LF_FUNC("spi") void spi_put(uint8_t byte) {

}

LF_FUNC("spi") uint8_t spi_get(void) {

}

LF_FUNC("spi") int spi_write(void *src, uint32_t length) {

	return lf_success;
}

LF_FUNC("spi") int spi_read(void *dst, uint32_t length) {

	return lf_success;
}

/* spi interrupt service routine */
void spi_isr(void) {

}
