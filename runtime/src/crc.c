#include <flipper.h>

/*                                      16   12   5
   This is the CCITT CRC 16 polynomial X  + X  + X  + 1. */
#define POLY 0x1021

uint16_t calcrc(const char *ptr, uint32_t count) {
	uint16_t crc;
	uint8_t i;
	crc = 0;
	while (count-- != 0) {
		crc = crc ^ (uint16_t)*ptr ++ << 8;
		i = 8;
		do {
			if (crc & 0x8000) {
				crc = crc << 1 ^ POLY;
			} else {
				crc = crc << 1;
			}
		} while(-- i);
	}
	return crc;
}

/* This function uses the CCITT crc16 algorithm. */
lf_crc_t lf_crc(const void *src, uint32_t length) {
	return calcrc(src, (uint32_t)length);
}
