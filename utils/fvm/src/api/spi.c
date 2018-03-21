#include <flipper.h>

int spi_configure() {
	printf("Configured the spi bus.\n");
	return lf_success;
}

void spi_enable(void) {
	printf("Enabling the spi bus.\n");
}

void spi_disable(void) {
	printf("Disabling the spi bus.\n");
}

uint8_t spi_ready(void) {
	printf("Checking if the spi bus is ready.\n");
	return lf_success;
}

void spi_put(uint8_t byte) {
	printf("Putting to the spi bus.\n");
}

uint8_t spi_get(void) {
	printf("Getting from the spi bus.\n");
	return lf_success;
}

int spi_push(void *source, uint32_t length) {
	printf("Pushing to the spi bus.\n");
	return lf_success;
}

int spi_pull(void *destination, uint32_t length) {
	printf("Pulling from the spi bus.\n");
	return lf_success;
}
