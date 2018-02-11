#include <flipper.h>

struct _lf_module *lf_module_create(char *name) {
	struct _lf_module *module = malloc(sizeof(struct _lf_module));
	lf_assert(module, failure, E_MALLOC, "Failed to allocate memory for new _lf_module.");
	module->name = strdup(name);
	return module;
failure:
	return NULL;
}
