#ifndef __lf_module_h__
#define __lf_module_h__

#include <flipper/types.h>
#include <flipper/fmr.h>

struct _lf_module {
	/* A string containing the module's name. */
	char *name;
	/* The version of the module. */
	lf_version_t version;
	/* The loaded index of the module on the device. */
	int idx;
	/* The module's interface. */
	void **interface;
};

#define LF_MODULE(sym, name, interface) \
	struct _lf_module sym = {           \
		name,                           \
		0,                              \
		-1,                             \
		interface                       \
	};

struct _lf_module *lf_module_create(const char *name, int idx);
int lf_module_release(struct _lf_module *module);

#endif
