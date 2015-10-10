#define __private_include__

#include <usb/usb.h>

#include <fvm/fvm.h>

#include <fmr/fmr.h>

#include <platform/fmr.h>

const struct _target fvm = {
	
	fvm_configure,
	
	fvm_call,
	
	fvm_invoke,
	
	fvm_push,
	
	fvm_pull
	
};

void fvm_configure(const struct _bus *bus) {
	
	/* ~ Configure the fvm's communication protocol. ~ */
	
	((struct _target *)(&fvm)) -> bus = bus;
	
	((struct _target *)(&fvm)) -> id = _self;
	
}

uint32_t fvm_call(void) {
	
	return 0;
	
}

uint32_t fvm_invoke(uint8_t object, uint8_t index, uint8_t argc, ...) {
	
	verbose("fvm (invoke):\n\n");
	
	/* ~ Construct a va_list to access variadic arguments. ~ */
	
	va_list argv;
	
	/* ~ Initialize the va_list that we created above. ~ */
	
	va_start(argv, argc);
	
	/* ~ Invoke the function on the selected target. ~ */
	
	return target_invoke(&fvm, object, index, argc, &argv);
	
}

uint32_t fvm_push(uint8_t object, uint8_t index, uint8_t argc, void *source, uint32_t length, ...) {
	
	verbose("fvm (push):\n\n");
	
	/* ~ Construct a va_list to access variadic arguments. ~ */
	
	va_list argv;
	
	/* ~ Initialize the va_list that we created above. ~ */
	
	va_start(argv, length);
	
	/* ~ Invoke the function on the selected target. ~ */
	
	return target_push(&fvm, object, index, argc, source, length, &argv);
	
}

void fvm_pull(uint8_t object, uint8_t index, uint8_t argc, void *destination, uint32_t length, ...) {
	
	verbose("fvm (pull):\n\n");
	
	/* ~ Construct a va_list to access variadic arguments. ~ */
	
	va_list argv;
	
	/* ~ Initialize the va_list that we created above. ~ */
	
	va_start(argv, length);
	
	/* ~ Invoke the function on the selected target. ~ */
	
	target_pull(&fvm, object, index, argc, destination, length, &argv);
	
}

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

void fdb_configure(void *configuration) {
	
	
	
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
	
	verbose("\tfdb ->\t");
	
	for (unsigned i = 0; i < length; i ++) { if ((i % 8 == 0) && (i != 0)) printf("\n\t\t"); printf("%02X  ", ((uint8_t *)(source))[i]); }
	
	verbose("\n\n");
	
	memcpy(&fmrpacket, source, length);
	
	//fmr_invoke(&fvm);
	
}

void fdb_pull(void *destination, uint32_t length) {
	
	verbose("\tfdb <-\t");
	
	system ("/bin/stty raw");
	
	while (length --) {
		
		char c = getchar();
		
		printf("%02X  ", c);
		
		if (c == '\n') break;
		
		*(char *)(destination ++) = c;
		
	}
	
	system ("/bin/stty cooked");
	
	verbose("\n\t\n");
	
}