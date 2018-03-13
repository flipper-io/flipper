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
	/* The module's jumptable. */
	void **jumptable;
};

#define LF_MODULE(sym, name, jumptable) \
	struct _lf_module sym = {           \
		name,                           \
		0,                              \
		-1,                             \
		jumptable                       \
	};

struct _lf_module *lf_module_create(char *name);
int lf_module_release(struct _lf_module *module);

#endif
