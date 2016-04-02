#define __private_include__
#include <flipper/at45.h>
#include <flipper/fmr.h>
#include <flipper/error.h>

void at45_configure(void) {

}

void at45_enable(void) {

	device_invoke(_at45, _at45_enable, NO_ARGS);

}

void at45_disable(void) {

	device_invoke(_at45, _at45_disable, NO_ARGS);

}

void at45_reset(void) {

	device_invoke(_at45, _at45_reset, NO_ARGS);

}

void at45_read(fsp address) {

	device_invoke(_at45, _at45_read, 2, hi16(address), lo16(address));

}

fsp at45_alloc(uint32_t length) {

	fsp address = device_invoke(_at45, _at45_alloc, 2, hi16(length), lo16(length));

	return address;

}

void at45_free(fsp pointer) {

	device_invoke(_at45, _at45_free, 2, hi16(pointer), lo16(pointer));

}

void at45_format(void) {

	device_invoke(_at45, _at45_format, NO_ARGS);

}

void at45_push(void *source, uint32_t length, fsp destination) {

	device_push(_at45, _at45_push, 2, source, length, hi16(destination), lo16(destination));

}

void at45_pull(void *destination, uint32_t length, fsp source) {

	device_pull(_at45, _at45_pull, 2, destination, length, hi16(source), lo16(source));

}

void *at45_dereference(fsp source, uint32_t length) {

	void *local = malloc(length);

	if (!local) {
		error_raise(E_NO_MEM, "");
		return 0;
	}

	at45_pull(local, length, source);

	return local;

}
