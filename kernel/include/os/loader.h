/* loader.h - Primitive type definitions for the Osmium loader. */

#ifndef __loader_h__
#define __loader_h__

#include <flipper.h>

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

int os_load_image(struct _lf_abi_header *header);

#endif
