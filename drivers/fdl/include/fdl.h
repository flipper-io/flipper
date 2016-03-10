#ifndef __fdl_h__
#define __fdl_h__

/* ~ The base address of the FDL configuration in NVM. ~ */
#define FDL_CONFIG_BASE 256

enum { fdl_config_base, fdl_config_brk };

#define fdl_config_offset(base, entity) (base + (entity * sizeof(uint32_t)))
#define fdl_write_config(object, config) at45_push(&object, sizeof(uint32_t), fdl_config_offset(FDL_CONFIG_BASE, config));
#define fdl_read_config(object, config) at45_pull(&object, sizeof(uint32_t), fdl_config_offset(FDL_CONFIG_BASE, config));

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* ~ Declare the virtual driver object. ~ */
extern const struct _fdl {

	void (* configure)(void);
	void *(* load)(uint16_t key);
	void (* launch)(uint16_t key);
	void (* resolve)(uint16_t key, const void *address);
	
} fdl;

#ifdef __private_include__

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _fdl_configure, _fdl_load, _fdl_launch, _fdl_resolve };

/* ~ Declare all function prototypes for this driver. ~ */
void fdl_configure(void);
void *fdl_load(uint16_t key);
void fdl_launch(uint16_t key);
void fdl_resolve(uint16_t key, const void *address);

#endif
#endif
