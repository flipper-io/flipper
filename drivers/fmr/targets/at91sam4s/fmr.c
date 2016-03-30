#define __private_include__
#include <flipper/fmr.h>
#include <flipper/drivers.h>
#include <flipper/platform/platform.h>

const void * const objects[] = { &host, &self, &device, &led, &button, &error, &usart, &spi, 0, &at45, &fs, &usb, &wifi, &io, &dbgu, &usart1, &fdl, &fmr };

/* ~ RAM cache of the last bound module. ~ */
struct _fmr_cache { uint16_t bundle; void *handle; } fmr_cache;

void fmr_configure(void) {

}

fmr_handle fmr_bind(uint16_t bundle) {

	/* ~ If the bundle requesting bind is cached, return its load address. ~ */
	if (fmr_cache.bundle == bundle) return (fmr_handle)(fmr_cache.handle);

	/* ~ Obtain the base address of the module. ~ */
	void *base = fdl_load(bundle);

	/* ~ Ensure the module was successfully loaded. ~ */
	if (!base) {
		error.raise(E_DL_LOAD, ERROR_STRING(E_DL_LOAD_S));
		return -1;
	}

	/* ~ Obtain the address of the module's configure function. ~ */
	void *configure = base + *(uintptr_t *)(base);

	/* ~ Hand control to the module to perform necessary configurations. ~ */
	((void (*)(void))(configure))();

	/* ~ Cache the module. ~ */
	fmr_cache.bundle = bundle; fmr_cache.handle = base;

	/* ~ Return the base address as the module's handle. ~ */
	return (uintptr_t)(base);

}

uint32_t fmr_invoke(fmr_handle handle, uint8_t index, uint8_t argc, ...) {

	/* ~ Mask the handle to obtain the base address of the module. ~ */
	void *base;

	//void *base = (void *)(handle & 0x1FFFFF);

	if (fmr_cache.bundle == handle) base = fmr_cache.handle; else fmr_bind(handle);

	/* ~ Obtain the address of the target function. ~ */
	void *function = base + *(uintptr_t *)(base + (index * sizeof(uintptr_t)));

	/* ~ The address of the variadic argument list is the address of the packet's body plus the 6 bytes needed for this function call. ~ */
	void *argv = fmrpacket.body + 6 * 2;

	/* ~ Call the target function. ~ */
	uint32_t result = internal_call(function, argc, argv);

	/* ~ Return the result to the caller. ~ */
	return result;

}

void *fmr_resolve(void *source, uint32_t length) {

	return NULL;

}
