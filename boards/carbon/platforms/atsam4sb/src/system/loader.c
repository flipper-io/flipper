/* Osmium loader implementation. */

#include <osmium.h>

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
  |          entry point         |               |
  +------------------------------+  0x0004       |
  |    sizeof(struct _module)    |               |
  +------------------------------+  0x0008       |
  |      &(struct _module)       |               |
  +------------------------------+  0x000c       | - HEADER
  |          .data size          |               |
  +------------------------------+  0x0010       |
  |         .data offset         |               |
  +------------------------------+  0x0014       |
  |          .bss size           |               |
  +------------------------------+  0x0018     --+
  |         .bss offset          |
  +------------------------------+  0x0018
  |            .got              |
  +------------------------------+  0x0010 + [0x0000]
  |            .text             |
  +------------------------------+  0x0010 + [0x000c]
  |            .data             |
  +------------------------------+  0x0010 + [0x0014]
  |             .bss             |
  +------------------------------*/

/* The data structure definition representing the ABI header above. */
struct _fld_header {
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

/* Mind the volatiles. */
void *volatile application_base = NULL;
void *volatile application_entry = NULL;

/* Loads a module or application located at the given address. */
int os_load(void *address) {
    /* Fail if there is currently an application loaded. */
    if (application_entry) {
        return lf_error;
    }
    /* Cast to the ABI header. */
    struct _fld_header *header = address;
    /* Set the image base. */
    application_base = address;

    printf("Loaded application at address %p.\n", application_base);
    /* Patch the GOT. */
    uintptr_t *got = (uintptr_t *)(application_base + header->got_offset);
    for (int i = 0; i < header->got_size / sizeof(uint32_t); i ++) {
        got[i] += (uintptr_t)application_base;
    }
    /* Set the entry point of the image. */
    application_entry = application_base + header->entry + 1;
    return lf_success;
}

void launch_application(void) {

    /* Wait until an application is ready to launch. */
	while(!application_base);

    /* Launch the application. */
	((void (*)(void))(application_entry))();

    /* If the base is vald, free the application memory. */
	if (application_base) {
		free((void *)(application_base));
	}

    /* Clear the registry pointers. */
	application_entry = NULL;
	application_base = NULL;
}
