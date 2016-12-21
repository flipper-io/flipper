#define __private_include__
#include <flipper/core.h>

/*                                      16   12   5
   This is the CCITT CRC 16 polynomial X  + X  + X  + 1. */
#define POLY 0x1021

uint16_t calcrc(char *ptr, int32_t count) {
	int  crc;
	char i;
	crc = 0;
	while (-- count >= 0) {
		crc = crc ^ (int) *ptr ++ << 8;
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
lf_id_t lf_checksum(void *source, lf_size_t length) {
	return calcrc(source, length);
}
