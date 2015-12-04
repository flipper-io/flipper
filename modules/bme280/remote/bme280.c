#define __private_include__

#include <bme280/bme280.h>

#include <platform/at91sam.h>

#define BME280_ADDRESS (0x77 << 16)

int32_t t_fine;

uint8_t bme280_read8(uint8_t reg) {
	
	uint8_t value = 0;
	
	i2c_get(BME280_ADDRESS | AT91C_TWI_IADRSZ_1_BYTE, reg, &value, sizeof(uint8_t));
	
	return value;
	
}

void bme280_write8(uint8_t reg, uint8_t value) {
	
	i2c_put(BME280_ADDRESS | AT91C_TWI_IADRSZ_1_BYTE, reg, &value, sizeof(uint8_t));
	
}

uint16_t bme280_read16(uint8_t reg) {
	
	uint16_t value = 0;
	
	i2c_get(BME280_ADDRESS | AT91C_TWI_IADRSZ_1_BYTE, reg, &value, sizeof(uint16_t));
	
	return little(value);
	
}

uint16_t bme280_read16le(uint8_t reg) {
	
	return little(bme280_read16(reg));
	
}

void bme280_calibrate(void) {
	
	bme280_calibration.dig_T1 = bme280_read16le(BME280_REGISTER_DIG_T1);
	
	bme280_calibration.dig_T2 = (int16_t)bme280_read16le(BME280_REGISTER_DIG_T2);
	
	bme280_calibration.dig_T3 = (int16_t)bme280_read16le(BME280_REGISTER_DIG_T3);
	
	bme280_calibration.dig_P1 = bme280_read16le(BME280_REGISTER_DIG_P1);
	
	bme280_calibration.dig_P2 = (int16_t)bme280_read16le(BME280_REGISTER_DIG_P2);
	
	bme280_calibration.dig_P3 = (int16_t)bme280_read16le(BME280_REGISTER_DIG_P3);
	
	bme280_calibration.dig_P4 = (int16_t)bme280_read16le(BME280_REGISTER_DIG_P4);
	
	bme280_calibration.dig_P5 = (int16_t)bme280_read16le(BME280_REGISTER_DIG_P5);
	
	bme280_calibration.dig_P6 = (int16_t)bme280_read16le(BME280_REGISTER_DIG_P6);
	
	bme280_calibration.dig_P7 = (int16_t)bme280_read16le(BME280_REGISTER_DIG_P7);
	
	bme280_calibration.dig_P8 = (int16_t)bme280_read16le(BME280_REGISTER_DIG_P8);
	
	bme280_calibration.dig_P9 = (int16_t)bme280_read16le(BME280_REGISTER_DIG_P9);
	
	bme280_calibration.dig_H1 = bme280_read8(BME280_REGISTER_DIG_H1);
	
	bme280_calibration.dig_H2 = (int16_t)bme280_read16le(BME280_REGISTER_DIG_H2);
	
	bme280_calibration.dig_H3 = bme280_read8(BME280_REGISTER_DIG_H3);
	
	bme280_calibration.dig_H4 = (bme280_read8(BME280_REGISTER_DIG_H4) << 4) | (bme280_read8(BME280_REGISTER_DIG_H4+1) & 0xF);
	
	bme280_calibration.dig_H5 = (bme280_read8(BME280_REGISTER_DIG_H5+1) << 4) | (bme280_read8(BME280_REGISTER_DIG_H5) >> 4);
	
	bme280_calibration.dig_H6 = (int8_t)bme280_read8(BME280_REGISTER_DIG_H6);
	
}

void bme280_configure(void) {
	
	bme280_calibrate();
	
	bme280_write8(BME280_REGISTER_CONTROLHUMID, 0x03);
	
	bme280_write8(BME280_REGISTER_CONTROL, 0x3F);
	
}

float bme280_temperature(void) {
	
	int32_t var1, var2;
	
	int32_t adc_T = bme280_read16(BME280_REGISTER_TEMPDATA);
	
	adc_T  << = 8;
	
	adc_T |= bme280_read8(BME280_REGISTER_TEMPDATA + 2);
	
	adc_T  >> = 4;
	
	var1  = ((((adc_T >> 3) - ((int32_t)bme280_calibration.dig_T1 << 1))) * ((int32_t)bme280_calibration.dig_T2)) >> 11;
	
	var2  = (((((adc_T >> 4) - ((int32_t)bme280_calibration.dig_T1)) *
			   
			((adc_T >> 4) - ((int32_t)bme280_calibration.dig_T1))) >> 12) *
			 
			((int32_t)bme280_calibration.dig_T3)) >> 14;
	
	t_fine = var1 + var2;
	
	float T  = (t_fine * 5 + 128) >> 8;
		
	return T / 100;
	
}

float bme280_pressure(void) {
	
	int64_t var1, var2, p;
	
	int32_t adc_P = bme280_read16(BME280_REGISTER_PRESSUREDATA);
	
	adc_P  << = 8;
	
	adc_P |= bme280_read8(BME280_REGISTER_PRESSUREDATA + 2);
	
	adc_P  >> = 4;
	
	var1 = ((int64_t)t_fine) - 128000;
	
	var2 = var1 * var1 * (int64_t)bme280_calibration.dig_P6;
	
	var2 = var2 + ((var1*(int64_t)bme280_calibration.dig_P5) << 17);
	
	var2 = var2 + (((int64_t)bme280_calibration.dig_P4) << 35);
	
	var1 = ((var1 * var1 * (int64_t)bme280_calibration.dig_P3) >> 8) + ((var1 * (int64_t)bme280_calibration.dig_P2) << 12);
	
	var1 = (((((int64_t) 1) << 47) + var1)) * ((int64_t)bme280_calibration.dig_P1) >> 33;
	
	if (var1 == 0) return 0;
	
	p = 1048576 - adc_P;
	
	p = (((p << 31) - var2) * 3125) / var1;
	
	var1 = (((int64_t)bme280_calibration.dig_P9) * (p >> 13) * (p >> 13)) >> 25;
	
	var2 = (((int64_t)bme280_calibration.dig_P8) * p) >> 19;
	
	p = ((p + var1 + var2) >> 8) + (((int64_t)bme280_calibration.dig_P7) << 4);
	
	return (float)p / 256;
}

float bme280_humidity(void) {
	
	int32_t adc_H = bme280_read16(BME280_REGISTER_HUMIDDATA);
	
	int32_t v_x1_u32r;
	
	v_x1_u32r = (t_fine - ((int32_t) 76800));
	
	v_x1_u32r = (((((adc_H << 14) - (((int32_t)bme280_calibration.dig_H4) << 20) -
					
				(((int32_t)bme280_calibration.dig_H5) * v_x1_u32r)) + ((int32_t)16384)) >> 15) *
				 
				(((((((v_x1_u32r * ((int32_t)bme280_calibration.dig_H6)) >> 10) *
					 
				(((v_x1_u32r * ((int32_t)bme280_calibration.dig_H3)) >> 11) + ((int32_t)32768))) >> 10) +
				   
				((int32_t)2097152)) * ((int32_t)bme280_calibration.dig_H2) + 8192) >> 14));
	
	v_x1_u32r = (v_x1_u32r - (((((v_x1_u32r >> 15) * (v_x1_u32r >> 15)) >> 7) *
							   
				((int32_t)bme280_calibration.dig_H1)) >> 4));
	
	v_x1_u32r = (v_x1_u32r < 0) ? 0 : v_x1_u32r;
	
	v_x1_u32r = (v_x1_u32r > 419430400) ? 419430400 : v_x1_u32r;
	
	float h = (v_x1_u32r >> 12);
	
	return h / 1024.0;
}