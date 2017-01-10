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
  |            &(main)           |               |
  +------------------------------+  0x0004       |
  |        sizeof(.module)       |               |
  +------------------------------+  0x0008       |
  |           &(.module)         |               |
  +------------------------------+  0x000c       |
  |          sizeof(.data)       |               |
  +------------------------------+  0x0010       | - HEADER
  |            &(.data)          |               |
  +------------------------------+  0x0014       |
  |          sizeof(.bss)        |               |
  +------------------------------+  0x0018       |
  |            &(.bss)           |               |
  +------------------------------+  0x001c       |
  |          sizeof(.got)        |               |
  +------------------------------+  0x0020     --+
  |            &(.got)           |
  +------------------------------+  [0x0008]
  |           .module            |
  +------------------------------+
  |            .text             |
  +------------------------------+  [0x0010]
  |            .data             |
  +------------------------------+  [0x0018]
  |             .bss             |
  +------------------------------*/

#define MAX_USER_MODULES 4

/* A data structure used to describe the parameters of a loaded module. */
struct _user_module {
    /* Array of the module's functions. */
    void **functions;
    /* The number of functions within the module. */
    int func_c;
    /* Pointer to the base of the module for deallocation purposes. */
    void *base;
};

/* A data structure used to keep track of actively registered user modules. */
struct _user_modules {
    /* An array of pointers to the user modules. */
    struct _user_module modules[MAX_USER_MODULES];
    /* The number of registered user modules. */
    volatile int count;
} user_modules;

/* Loads an application into RAM. */
int os_load_application(void *base, struct _lf_abi_header *header) {
    /* Obtain the address of the application's main function. */
    void *_main = base + header -> entry;
    /* Allocate the application's stack. */
    os_stack_t *_stack = malloc(APPLICATION_STACK_SIZE_WORDS * sizeof(uint32_t));
    if (!_stack) {
        error_raise(E_MALLOC, NULL);
        return lf_error;
    }
    /* Register the task for launch. */
    struct _os_task *task = os_task_create(_main, _stack, APPLICATION_STACK_SIZE_WORDS * sizeof(uint32_t));
    /* Verify that the task was created successfully. */
    if (!task) {
        error_raise(E_UNIMPLEMENTED, NULL);
        /* Free the memory allocated for the application's stack. */
        free(_stack);
        return lf_error;
    }
    /* Set the task's base address. */
    task -> base = base;
    /* Launch the task. */
    os_task_next();
    return lf_success;
}

/* Loads a module into RAM. */
int os_load_module(void *base, struct _lf_abi_header *header) {
    /* If there are no more user module slots to allocate, return with error. */
    if (user_modules.count >= MAX_USER_MODULES) {
        error_raise(E_UNIMPLEMENTED, NULL);
        return lf_error;
    } else {
        fmr_module index = user_modules.count;
        /* Allocate the user module. */
        struct _user_module *module = &user_modules.modules[user_modules.count ++];
        /* Save the module struct. */
        module -> functions = base + header -> module_offset;
        /* Store the number of functions that exist within the module. */
        module -> func_c = (header -> module_size / sizeof(uintptr_t));
        /* Save the base address of the module. */
        module -> base = base;
        /* Send the index back to the host. */
        return index;
    }
}

/* Loads an image into RAM. */
int os_load_image(void *base) {
    /* Cast the base pointer to obtain the ABI header. */
    struct _lf_abi_header *header = base;

    /* Patch the function pointers in the module structure. */
    void **_functions = base + header -> module_offset;
    for (int i = 0; i < header -> module_size / sizeof(uintptr_t); i ++) {
        _functions[i] += (uintptr_t)base;
    }

    /* Patch the Global Offset Table of the image. */
    uintptr_t *got = base + header -> got_offset;
    for (int i = 0; i < header -> got_size / sizeof(uintptr_t); i ++) {
        got[i] += (uintptr_t)base;
    }

    /* Zero the BSS. */
    uint32_t *bss = base + header -> got_offset;
    for (int i = 0; i < header -> bss_size; i ++) {
        bss[i] = 0;
    }

    if (header -> entry) {
        /* If the image has an entry point, load it as an application. */
        if (os_load_application(base, header) < lf_success) {
            goto failure;
        }
    } else {
        /* If not, load the image as a module. */
        if (os_load_module(base, header) < lf_success) {
            goto failure;
        }
    }

    return lf_success;

failure:
    /* Free the memory allocated to load the image. */
    free(base);
    return lf_error;
}

/* Handles the invocation of user functions. */
fmr_return fmr_perform_user_invocation(struct _fmr_invocation_packet *packet) {
    /* Ensure that the index is within bounds. */
    if (packet -> call.index >= user_modules.count) {
        return lf_error;
    }
    /* Get a pointer to the module. */
    struct _user_module *module = &user_modules.modules[packet -> call.index];
    /* Ensure that the function is within bounds. */
    if (packet -> call.function >= module -> func_c) {
        return lf_error;
    }
    /* Dereference a pointer to the target function. */
    const void *address = module -> functions[packet -> call.function];
    /* Ensure that the function address is valid. */
    if (!address) {
        error_raise(E_RESOULTION, NULL);
        return lf_error;
    }
    /* Perform the function call internally. */
    return fmr_call(address, packet -> call.argc, packet -> call.types, packet -> call.parameters);
}
