#include <flipper.h>

struct _lf_module *dyld_register(struct _lf_device *device, char *module, void **jumptable) {
    struct _lf_module *m = lf_module_create(module, lf_ll_count(device->modules), jumptable);
    lf_assert(m, failure, E_NULL, "Failed to register module '%s'.", module);
    lf_ll_append(&device->modules, m, lf_module_release);
    return m;
failure:
    return NULL;
}

/* Load a module onto the device. */
int dyld_load(struct _lf_device *device, void *src, size_t len) {
    return lf_success;
}

/* Get the module index on the device. */
struct _lf_module *dyld_module(struct _lf_device *device, char *module) {
    lf_assert(device, failure, E_NULL, "No device provided to '%s'.", __PRETTY_FUNCTION__);
    lf_assert(module, failure, E_NULL, "No module provided to '%s'.", __PRETTY_FUNCTION__);

    int count = lf_ll_count(device->modules);
    for (int i = 0; i < count; i ++) {
        struct _lf_module *m = lf_ll_item(device->modules, i);
        if (!strcmp(m->name, module)) return m;
    }

    #warning This is not optimal.
    if (device != &THIS_DEVICE) {
        /* If the module hasn't already been registered, try to register it. */
        int idx = lf_dyld(device, module);
        lf_assert(idx != lf_error, failure, E_MODULE, "Failed to find counterpart for module '%s' on device '%s'.", module, device->name);
        return dyld_register(device, module, NULL);
    }

failure:
    return NULL;
}

/* Unload a module from the device. */
int dyld_unload(struct _lf_device *device, char *module) {
    struct _lf_module *m = dyld_module(device, module);
    lf_assert(module, failure, E_NULL, "No module '%s' loaded on device '%s'.", module, device->name);
    lf_ll_remove(&device->modules, m);
    return lf_success;
failure:
    return lf_error;
}
