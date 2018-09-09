#include "libflipper.h"

int dyld_register(struct _lf_device *device, struct _lf_module *module) {
    lf_assert(device, E_NULL, "invalid device");
    lf_assert(module, E_NULL, "invalid module");

#warning This is not a good way to increment the count.
    if (module->idx == -1) module->idx = lf_ll_count(device->modules);
        // lf_debug("Registering module '%s' with index '%i'.", module->name, module->idx);
#warning Need to handle reloading an existing moudle here.
    return lf_ll_append(&device->modules, module, lf_module_release);

fail:
    return lf_error;
}

/* Load a module onto the device. */
int dyld_load(struct _lf_device *device, void *src, size_t len) {
    return lf_success;
}

/* Get the module index on the device. */
struct _lf_module *dyld_module(struct _lf_device *device, const char *module) {
    lf_assert(device, E_NULL, "invalid device");
    lf_assert(module, E_NULL, "invalid module");

    int count = lf_ll_count(device->modules);
    for (int i = 0; i < count; i++) {
        struct _lf_module *m = lf_ll_item(device->modules, i);
        if (!strcmp(m->name, module)) return m;
    }

    /* If the module hasn't already been registered, try to register it. */
    int idx;
    lf_assert(lf_dyld(device, module, &idx), E_MODULE, "Failed to find counterpart for module '%s' on device '%s'.",
              module, device->name);

    struct _lf_module *m = lf_module_create(module, idx);
    lf_assert(module, E_NULL, "Failed to create new module '%s'.", module);

    int e = dyld_register(device, m);
    lf_assert(e, E_MODULE, "Failed to register module '%s'.", module);
    return m;

fail:
    return NULL;
}

/* Unload a module from the device. */
int dyld_unload(struct _lf_device *device, char *module) {
    lf_assert(device, E_NULL, "invalid device");
    lf_assert(module, E_NULL, "invalid module");

    struct _lf_module *m = dyld_module(device, module);
    lf_assert(module, E_NULL, "no module '%s' loaded on device '%s'.", module, device->name);
    lf_ll_remove(&device->modules, m);
    return lf_success;

fail:
    return lf_error;
}
