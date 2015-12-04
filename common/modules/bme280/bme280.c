#define __private_include__

#include <flipper/flipper.h>

#include <modules/bme280/bme280.h>

#define BME280_ADDRESS (0x77 << 16)

struct _bme280 bme280 = {
	
	bme280_configure,
	
	bme280_temperature,
	
	bme280_pressure,
	
	bme280_humidity
	
};