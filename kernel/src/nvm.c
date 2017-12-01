#define __private_include__
#include <flipper/is25lp.h>

/* - Common NVM driver implementation. - */

/* The symbols 'nvm_configure', 'nvm_enable', 'nvm_disable', and 'nvm_reset' are declared in $platforms/nvm/nvm.c. */

void nvm_read(nvm_p address) {

}

uint8_t nvm_get(void) {
	return 0;
}

/* The symbols 'nvm_alloc' and 'nvm_free' are declared in alloc.c. */

/* The symbol 'nvm_format' is declared in $platforms/nvm/nvm.c. */

void nvm_copy(nvm_p destination, nvm_p source, lf_size_t length) {

}

void nvm_push(void *source, uint32_t length, nvm_p destination) {

}

void nvm_pull(void *destination, uint32_t length, nvm_p source) {

}

void *nvm_dereference(nvm_p source, uint32_t length) {
	return NULL;
}
