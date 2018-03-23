/* The Osmium loader implementation. */

#include <os/loader.h>
#include <os/scheduler.h>

/*
 * The loader handles the loading of moudles and applications into program memory
 * and RAM for debugging. Flipper moudles and applications must be compiled
 * to comply with the Flipper ABI (Application Binary Interface) such that
 * the loader can properly handle the relocation of the data segment for modules
 * and applications loaded into ROM. RAM loaded modules and applications do not
 * require relocation of their .data and .bss sections, and thus be compiled as
 * position independant, and without a GOT (Global Offset Table).
 *
 */


 /* FDL Module/Application ABI Specification */

 /*------------------------------+  0x0000     --+
  |          sizeof(name)        |               |
  +------------------------------+  0x0004       |
  |            &(name)           |               |
  +------------------------------+  0x0008       |
  |            &(main)           |               |
  +------------------------------+  0x000c       |
  |        sizeof(.module)       |               |
  +------------------------------+  0x0010       |
  |           &(.module)         |               |
  +------------------------------+  0x0014       |
  |          sizeof(.data)       |               |
  +------------------------------+  0x0018       | - HEADER
  |            &(.data)          |               |
  +------------------------------+  0x001c       |
  |          sizeof(.bss)        |               |
  +------------------------------+  0x0020       |
  |            &(.bss)           |               |
  +------------------------------+  0x0024       |
  |          sizeof(.got)        |               |
  +------------------------------+  0x0028     --+
  |            &(.got)           |
  +------------------------------+  [0x000c]
  |           .module            |
  +------------------------------+
  |            .text             |
  +------------------------------+  [0x0018]
  |            .data             |
  +------------------------------+  [0x0020]
  |             .bss             |
  +------------------------------*/

struct _lf_ll *tasks;

/* Called when an app finishes running. */
void os_app_exit(struct _lf_abi_header *header) {
	free(header);
}

/* Loads an application into RAM. */
int os_load_application(struct _lf_abi_header *header) {
	struct _os_task *task = NULL;

#warning Check here if the task exists.

	void *_main = header + header->entry;
	task = os_task_create(_main, os_app_exit, header, APPLICATION_STACK_SIZE_WORDS * sizeof(uint32_t));
	lf_assert(task, failure, E_NULL, "Failed to allocate memory for task");

	/* Add the task. */
	os_task_add(task);
	/* Launch the task. */
	os_task_next();

	return lf_success;
failure:
	if (task) os_task_release(task);
	return lf_error;
}

/* Loads an image into RAM. */
int os_load_image(struct _lf_abi_header *header) {
	lf_assert(header, failure, E_NULL, "NULL module header provided for module load.");

#warning Make sure the module is valid here.

	/* Patch the function pointers in the module structure. */
	void **_functions = (void **)header + header->module_offset;
	for (unsigned i = 0; i < header->module_size / sizeof(void **); i ++) {
		_functions[i] += (uintptr_t)header;
	}

	/* Patch the Global Offset Table of the image. */
	void **_got = (void **)header + header->got_offset;
	for (unsigned i = 0; i < header->got_size / sizeof(void **); i ++) {
		_got[i] += (uintptr_t)header;
	}

	/* Zero the BSS. */
	uint32_t *_bss = (void *)header + header->got_offset;
	for (unsigned i = 0; i < header->bss_size / sizeof(uint32_t); i ++) {
		_bss[i] = 0;
	}

#warning Actually load the module here.

	if (header->entry) {
		/* Load an application. */

	} else {
		/* Load a module. */
		struct _lf_module *_module = (struct _lf_module *)header + header->module_offset;
		dyld_register(lf_get_current_device(), _module);
	}

	return lf_success;
failure:
	free(header);
	return lf_error;
}
