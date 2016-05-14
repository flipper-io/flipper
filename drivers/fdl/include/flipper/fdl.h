#ifndef __fdl_h__
#define __fdl_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

/* Define the base address of the FDL configuration in NVM. */
#define FDL_CONFIG_BASE 256

enum { fdl_config_base, fdl_config_brk };
#define fdl_config_offset(base, entity) (base + (entity * sizeof(uint32_t)))
#define fdl_write_config(object, config) nvm_push(&object, sizeof(uint32_t), fdl_config_offset(FDL_CONFIG_BASE, config));
#define fdl_read_config(object, config) nvm_pull(&object, sizeof(uint32_t), fdl_config_offset(FDL_CONFIG_BASE, config));

#define RW_SIZE_OFFSET  0x0004
#define GOT_ADDR_OFFSET 0x0008
#define DATA_ADDR       0x0010
#define DRIVER_OFFSET   0x0014

/* Declare the virtual interface for this module. */
extern const struct _fdl {

	/* Configures the FDL module. */
	void (* configure)(void);

	/* Dynamically loads a Flipper module from the device's filesystem. */
	void *(* load)(uint16_t key);

	/* Launches an application that has been loaded by FDL. */
	void (* launch)(uint16_t key);

	/* ??? */
	void (* resolve)(uint16_t key, const void *address);
	
} fdl;

#ifdef __private_include__

/* Declare the FMR overlay for this driver. */
enum { _fdl_configure, _fdl_load, _fdl_launch, _fdl_resolve };

/* Declare each prototype for all functions within this driver. */
void fdl_configure(void);
void *fdl_load(uint16_t key);
void fdl_launch(uint16_t key);
void fdl_resolve(uint16_t key, const void *address);

#endif
#endif
