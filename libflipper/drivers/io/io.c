#define __private_include__

#include <io/io.h>

#include <fmr/fmr.h>

void io_configure(void) {
	
	
	
}

void io_set_direction(uint8_t pin, uint8_t direction) {
	
	host.invoke(_io, _io_set_direction, 4, fmr_argument(pin), fmr_argument(direction));
	
}

void io_write(uint8_t pin, uint16_t value) {
	
	host.invoke(_io, _io_write, 4, fmr_argument(pin), fmr_argument(value));
	
}

uint16_t io_read(uint8_t pin) {
	
	return (uint16_t)(host.invoke(_io, _io_read, 2, little(pin), 0));
	
}