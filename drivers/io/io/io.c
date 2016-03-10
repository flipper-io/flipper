#define __private_include__
#include <io/io.h>
#include <fmr/fmr.h>

void io_configure(void) {

}

void io_set_direction(uint8_t pin, uint8_t direction) {
	
	host.invoke(_io, _io_set_direction, fmr_args(pin, direction));
	
}

void io_write(uint8_t pin, uint16_t value) {
	
	host.invoke(_io, _io_write, fmr_args(pin, value));
	
}

uint16_t io_read(uint8_t pin) {
	
	uintres_t value = host.invoke(_io, _io_read, fmr_args(pin));
	
	return (uint16_t)(value);
	
}