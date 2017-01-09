/* The Osmium loader implementation. */

#define __private_include__
#include <osmium.h>
#include <loader.h>
#include <scheduler.h>

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

/* Loads a module or application located at the given address. */
int os_load(void *base) {
    /* Cast to the ABI header. */
    struct _fld_header *header = base;

    /* Patch the GOT. */
    uintptr_t *got = (uintptr_t *)(base + header -> got_offset);
    for (int i = 0; i < header -> got_size / sizeof(uint32_t); i ++) {
      got[i] += (uintptr_t)base;
    }

    /* Obtain the address of the entry point of the image. */
    void *application_entry = base + header -> entry + THUMB_BIT;

    /* Register the task for launch. */
    struct _os_task *task = os_task_create(application_entry, malloc(256), 256);
    /* Set the task's base address. */
    task -> base = base;
    printf("Allocated task at address %p.\n", base);
    /* Start the task. */
    os_task_next();

    return lf_success;
}
