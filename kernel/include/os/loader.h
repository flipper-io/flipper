/* loader.h - Primitive type definitions for the Osmium loader. */

#ifndef __loader_h__
#define __loader_h__

#include <flipper/libflipper.h>

/* The default stack size for applications. */
#define APPLICATION_STACK_SIZE_WORDS 256

#define MAX_USER_MODULES 4

/* The data structure definition representing the ABI header above. */
struct _lf_abi_header {
	uint32_t name_size;
	uint32_t name_offset;
	uint32_t entry;
	uint32_t module_size;
	uint32_t module_offset;
	uint32_t data_size;
	uint32_t data_offset;
	uint32_t bss_size;
	uint32_t bss_offset;
	uint32_t got_size;
	uint32_t got_offset;
};

/* A data structure used to describe the parameters of a loaded module. */
struct _user_module {
	/* The identifier of the user module. */
	lf_crc_t identifier;
	/* Array of the module's functions. */
	void **functions;
	/* The number of functions within the module. */
	int func_c;
	/* Pointer to the base of the module for deallocation purposes. */
	void *base;
};

/* A data structure used to keep track of actively registered user modules. */
extern struct _user_modules {
	/* An array of pointers to the user modules. */
	struct _user_module modules[MAX_USER_MODULES];
	/* The number of registered user modules. */
	volatile int count;
} user_modules;

int os_load_image(void *base);

int os_get_module_index(lf_crc_t identifier);

#endif
