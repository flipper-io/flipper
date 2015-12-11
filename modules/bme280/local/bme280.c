#define __private_include__

#include <bme280/bme280.h>

void bme280_configure(void) {
	
	host.invoke(_bme280, _bme280_configure, NO_ARGS);
	
}

float bme280_temperature(void) {
	
	uint32_t temperature = host.invoke(_bme280, _bme280_temperature, NO_ARGS);
	
	return *(float *)(&temperature);
	
}

float bme280_pressure(void) {
	
	uint32_t pressure = host.invoke(_bme280, _bme280_pressure, NO_ARGS);
	
	return *(float *)(&pressure);
}

float bme280_humidity(void) {
	
	uint32_t humidity = host.invoke(_bme280, _bme280_humidity, NO_ARGS);
	
	return *(float *)(&humidity);
}