#define __private_include__

#include <fvm/fvm.h>

const struct _target fvm = {
	
	fvm_configure,
	
	fvm_invoke,
	
	fvm_push,
	
	fvm_pull
	
};

/* ------------------------ FDB ------------------------ */

const struct _bus fdb = {
	
	fdb_configure,
	
	fdb_enable,
	
	fdb_disable,
	
	fdb_ready,
	
	fdb_put,
	
	fdb_get,
	
	fdb_push,
	
	fdb_pull
	
};

void fvm_configure(const struct _bus *bus) {
	
	/* ~ Configure the fvm's communication protocol. ~ */
	
	((struct _target *)(&fvm)) -> bus = bus;
	
}

uint32_t fvm_invoke(uint8_t object, uint8_t index, uint8_t argc, ...) {
	
	verbose("fvm ");
	
	/* ~ Construct a va_list to access variadic arguments. ~ */
	
	va_list argv;
	
	/* ~ Initialize the va_list that we created above. ~ */
	
	va_start(argv, argc);
	
	/* ~ Invoke the function on the selected target. ~ */
	
	return target_invoke(&fvm, object, index, argc, &argv);
	
}

uint32_t fvm_push(uint8_t object, uint8_t index, uint8_t argc, void *source, uint32_t length, ...) {
	
	return 0;
	
}

void fvm_pull(uint8_t object, uint8_t index, uint8_t argc, void *destination, uint32_t length, ...) {
	
	
	
}

/* ------------------------ fdb ------------------------ */

void fdb_configure(uint16_t baud) {
	
	
	
}

void fdb_enable(void) {
	
	
	
}

void fdb_disable(void) {
	
	
	
}

bool fdb_ready(void) {
	
	return 0;
	
}

void fdb_put(uint8_t byte) {
	
	
	
}

uint8_t fdb_get(void) {
	
	return 0;
	
}

void fdb_push(void *source, uint32_t length) {
	
	/* ~ Print debugging information. ~ */
	
	printf("-> push -> fdb:\n\t");
	
	for (unsigned i = 0; i < length; i ++) { if (i % 8 == 0) printf("\n\t"); printf("%02X ", ((uint8_t *)(source))[i]); }
	
	printf("\n\n");
	
}

void fdb_pull(void *destination, uint32_t length) {
	
	
	
}