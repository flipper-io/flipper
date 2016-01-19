#ifndef fdl_h

#define fdl_h

#include <flipper/types.h>

/* ~ The base address of the FDL configuration in NVM. ~ */

#define FDL_CONFIG_BASE 256

enum { FDL_STARTUP_PROGRAM };

#define config_offset(base, entity) (base + (entity * sizeof(uint32_t)))

extern const struct _fdl {

	void (* configure)(void);

	void *(* load)(uint16_t key);
	
	void (* resolve)(uint16_t key, const void *address);

} fdl;

#ifdef __private_include__

enum { _fdl_configure, _fdl_load, _fdl_resolve };

void fdl_configure(void);

void *fdl_load(uint16_t key);

void fdl_resolve(uint16_t key, const void *address);

#endif 

#endif