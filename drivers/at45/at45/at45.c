#define __private_include__
#include <flipper/at45.h>
#include <flipper/fmr.h>
#include <flipper/error.h>

void at45_configure(void) {

}

void at45_enable(void) {

	device.invoke(_at45, _at45_enable, NO_ARGS);

}

void at45_disable(void) {

	device.invoke(_at45, _at45_disable, NO_ARGS);

}

void at45_reset(void) {

	device.invoke(_at45, _at45_reset, NO_ARGS);

}

fsp at45_alloc(uint32_t length) {

	fsp address = device.invoke(_at45, _at45_alloc, 2, hi16(length), lo16(length));

	return address;

}

void at45_free(fsp pointer) {

	device.invoke(_at45, _at45_free, 2, hi16(pointer), lo16(pointer));

}

void at45_format(void) {

	device.invoke(_at45, _at45_format, NO_ARGS);

}

void at45_push(void *source, uint32_t length, fsp destination) {

	device.push(_at45, _at45_push, 2, source, length, hi16(destination), lo16(destination));

}

void at45_pull(void *destination, uint32_t length, fsp source) {

	device.pull(_at45, _at45_pull, 2, destination, length, hi16(source), lo16(source));

}

void *at45_dereference(fsp source, uint32_t length) {

	void *local = malloc(length);

	if (!local) {
		error.raise(E_NO_MEM, ERROR_STRING(E_NO_MEM_S));
		return 0;
	}

	at45_pull(local, length, source);

	return local;

}
