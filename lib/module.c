#include "libflipper.h"

struct _lf_module *lf_module_create(const char *name, uint16_t idx) {
    lf_assert(name, E_NULL, "No name");
    lf_assert(strlen(name) < 16, E_OVERFLOW, "Module name '%s' is invalid. Module names must be 16 characters or less.", name);
    struct _lf_module *module = calloc(1, sizeof(struct _lf_module));
    lf_assert(module, E_MALLOC, "Failed to allocate memory for new _lf_module.");
    size_t len = strlen(name) + 1;
    module->name = malloc(len);
    strcpy(module->name, name);
    module->idx = idx;
    return module;
fail:
    return NULL;
}

void lf_module_release(void *_module) {
    struct _lf_module *module = _module;
    lf_assert(module, E_NULL, "invalid module");
    free(module->name);
    free(module);
fail:
    return;
}
