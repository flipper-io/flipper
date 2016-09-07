#ifndef __osmium_h__
#define __osmium_h__

#ifdef __private_include__

/* Initializes platform specific periphery and prepares system for kernel configuration. */
extern void system_init();
/* Deinitializes platform specific periphery and prepares system for shutdown. */
extern void system_deinit();

#endif
#endif
