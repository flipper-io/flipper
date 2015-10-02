#ifndef libflipper_h

#define libflipper_h

#include <flipper/types.h>

/* ~ Define the flippper control type. ~ */

enum { FLIPPER_SOURCE_USB, FLIPPER_SOURCE_NETWORK, FLIPPER_SOURCE_FVM };

enum { _flipper_configure };

extern const struct _flipper {
	
	void (* configure)(void);
	
	void (* attach)(uint8_t source, ...);
	
} flipper;

#ifdef __private_include__

extern void flipper_configure(void);

extern void flipper_attach(uint8_t source, ...);

#endif

#endif