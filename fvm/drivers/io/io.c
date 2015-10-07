#define __private_include__

#include <io/io.h>

#include <fmr/fmr.h>

void io_configure(void) {
	
	
	
}

void io_set_direction(uint8_t pin, uint8_t direction) {
	
	device.invoke(_io, _io_set_direction, 2, pin, direction);
	
}

void io_write(uint8_t pin, uint16_t value) {
	
	device.invoke(_io, _io_write, 2, pin, value);
	
}

uint16_t io_read(uint8_t pin) {
	
	return (uint16_t)(device.invoke(_io, _io_read, 1, pin));
	
}