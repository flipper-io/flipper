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

#define MAX_USER_MODULES 4

struct _user_module {
    /* Pointer to the module struct. */
    void **functions;
    /* The number of functions in the module. */
    int func_c;
    /* Base of the module for deallocation purposes. */
    void *base;
};

struct _user_modules {
    /* An array of pointers to the user modules. */
    struct _user_module modules[MAX_USER_MODULES];
    /* The number of registered user modules. */
    volatile int count;
} user_modules;

/* Loads a module or application located at the given address. */
int os_load(void *base) {
    /* Cast to the ABI header. */
    struct _fld_header *header = base;

    /* Patch the GOT. */
    uintptr_t *got = (uintptr_t *)(base + header -> got_offset);
    for (int i = 0; i < header -> got_size / sizeof(uint32_t); i ++) {
      got[i] += (uintptr_t)base;
    }

    /* If the image has an entry point, treat it as an application. */
    if (header -> entry) {
        /* Obtain the address of the entry point of the image. */
        void *application_entry = base + header -> entry + THUMB_BIT;

        /* Register the task for launch. */
        struct _os_task *task = os_task_create(application_entry, malloc(256), 256);
        /* Set the task's base address. */
        task -> base = base;
        /* Start the task. */
        os_task_next();
    } else {

        if (user_modules.count >= MAX_USER_MODULES) {
            error_raise(E_MODULE, NULL);
            free(base);
            return lf_error;
        } else {
            /* Obtain the module structure. */
            void **_struct = base + header -> module_offset;
            /* Patch the function addresses. */
            for (volatile int i = 0; i < header -> module_size / sizeof(uintptr_t); i ++) {
                _struct[i] += (uintptr_t)base;
            }
            /* Allocate the user module. */
            fmr_module index = user_modules.count;
            struct _user_module *module = &user_modules.modules[user_modules.count ++];
            /* Save the module struct. */
            module -> functions = _struct;
            /* Set the number of functions. */
            module -> func_c = (header -> module_size / sizeof(uintptr_t));
            /* Save the base. */
            module -> base = base;
            /* Send the index back to the host. */
            return index;
        }

    }

    return 0xdeadbeef;
}

/* Experimental: User funtion invocation handler. */
fmr_return fmr_perform_user_invocation(struct _fmr_invocation_packet *packet) {
    /* Check index boundary. */
    if (packet -> call.index >= user_modules.count) {
        return 0;
    }
    /* Get module. */
    struct _user_module *module = &user_modules.modules[packet -> call.index];
    /* Check function boundary. */
    if (packet -> call.function >= module -> func_c) {
        return 0;
    }
    /* Dereference and return a pointer to the target function. */
    const void *address = module -> functions[packet -> call.function];
    /* Ensure that the function address is valid. */
    if (!address) {
        error_raise(E_RESOULTION, NULL);
        return 0;
    }
    /* Perform the function call internally. */
    return fmr_call(address, packet -> call.argc, packet -> call.types, packet -> call.parameters);
}
