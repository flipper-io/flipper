#define __private_include__
#include <flipper/core.h>

#define POLY 0x8408

/* This function uses the CCITT crc16 algorithm. */
lf_id_t lf_checksum(void *source, size_t length) {
	uint8_t *_data = (uint8_t *)source;
	uint32_t data;
	uint32_t crc = 0xffff;
	/* If the length is zero, return zero. */
	if (length == 0) {
		return (~crc);
	}
	/* Perform the cyclic redundancy check. */
	do {
		for (uint8_t i = 0, data = (uint32_t)0xff & *_data ++; i < 8; i ++, data >>= 1) {
			if ((crc & 0x0001) ^ (data & 0x0001)) {
				crc = (crc >> 1) ^ POLY;
			} else {
				crc >>= 1;
			}
		}
	} while (-- length);
	/* Convert the result of the above operation into CCITT crc16. */
	crc = ~crc;
	data = crc;
	crc = (crc << 8) | (data >> 8 & 0xff);
	return crc;
}
