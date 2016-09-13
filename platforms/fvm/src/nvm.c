#define __private_include__
#include <private/nvm.h>
#include <platform/fvm.h>

/* Define global state used for benchmarking. */
size_t nvm_read_ops, nvm_write_ops, nvm_read_bytes, nvm_written_bytes;

int nvm_configure(void) {
	return lf_success;
}

void nvm_enable(void) {

}

void nvm_disable(void) {

}

void nvm_reset(void) {

}

void nvm_write(nvm_p address) {

}

void nvm_put(uint8_t byte) {

}

void nvm_read(nvm_p address) {

}

uint8_t nvm_get(void) {

	return 0;
}

nvm_p nvm_alloc(lf_size_t length) {
	return 0;
}

void nvm_free(nvm_p pointer) {

}

void nvm_format(void) {
	memset(v_nvm, 0xFF, NVM_SIZE);
}

void nvm_copy(nvm_p destination, nvm_p source, lf_size_t length) {
	memcpy((void *)(v_nvm + destination), (void *)(v_nvm + source), length);
}

void nvm_push(void *source, lf_size_t length, nvm_p destination) {
	nvm_write_ops ++;
	nvm_written_bytes += length;
	memcpy((void *)(v_nvm + destination), source, length);
}

void nvm_pull(void *destination, lf_size_t length, nvm_p source) {
	nvm_read_ops ++;
	nvm_read_bytes += length;
	memcpy(destination, (void *)(v_nvm + source), length);
}

void *nvm_dereference(nvm_p source, lf_size_t length) {
	void *address = (void *)malloc(length);
	nvm_pull(address, length, source);
	return address;
}
