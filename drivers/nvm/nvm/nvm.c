#define __private_include__
#include <flipper/nvm.h>
#include <flipper/fmr.h>

void nvm_configure(void) {

}

void nvm_enable(void) {

	device_invoke(_nvm, _nvm_enable, NO_ARGS);
}

void nvm_disable(void) {

	device_invoke(_nvm, _nvm_disable, NO_ARGS);

}

void nvm_reset(void) {

	device_invoke(_nvm, _nvm_reset, NO_ARGS);

}

void nvm_read(fsp address) {

	device_invoke(_nvm, _nvm_read, 2, hi16(address), lo16(address));

}

uint8_t nvm_get(void) {

	return device_invoke(_nvm, _nvm_get, NO_ARGS);

}

fsp nvm_alloc(fmr_size_t length) {

	return device_invoke(_nvm, _nvm_alloc, 2, hi16(length), lo16(length));

}

void nvm_free(fsp pointer) {

	device_invoke(_nvm, _nvm_free, 2, hi16(pointer), lo16(pointer));

}

void nvm_format(void) {

	device_invoke(_nvm, _nvm_format, NO_ARGS);

}

void nvm_push(void *source, fmr_size_t length, fsp destination) {

	device_push(_nvm, _nvm_push, 2, source, length, hi16(destination), lo16(destination));

}

void nvm_pull(void *destination, fmr_size_t length, fsp source) {

	device_pull(_nvm, _nvm_pull, 2, destination, length, hi16(source), lo16(source));

}

void *nvm_dereference(fsp source, fmr_size_t length) {

	void *local = malloc(length);

	if (!local) {
		error_raise(E_NO_MEM, "");
		return 0;
	}

	nvm_pull(local, length, source);

	return local;

}
