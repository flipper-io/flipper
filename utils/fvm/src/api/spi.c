#include "libflipper.h"

LF_FUNC int spi_configure() {
    printf("Configured the spi bus.\n");
    return lf_success;
}

LF_FUNC void spi_enable(void) {
    printf("Enabling the spi bus.\n");
}

LF_FUNC void spi_disable(void) {
    printf("Disabling the spi bus.\n");
}

LF_FUNC uint8_t spi_ready(void) {
    printf("Checking if the spi bus is ready.\n");
    return lf_success;
}

LF_FUNC void spi_put(uint8_t byte) {
    printf("Putting to the spi bus.\n");
}

LF_FUNC uint8_t spi_get(void) {
    printf("Getting from the spi bus.\n");
    return lf_success;
}

LF_FUNC int spi_write(void *src, uint32_t length) {
    printf("Pushing to the spi bus.\n");
    return lf_success;
}

LF_FUNC int spi_read(void *dst, uint32_t length) {
    printf("Pulling from the spi bus.\n");
    return lf_success;
}
