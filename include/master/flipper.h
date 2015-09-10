#ifndef __flipper_h__

#define __flipper_h__

#include <flipper/types.h>

enum { FLIPPER_SOURCE_USB, FLIPPER_SOURCE_NETWORK };

enum { _flipper_configure };

extern const struct _flipper {
	
	void (* configure)(void);
	
	void (* attach)(char *device, uint8_t source, ...);
	
} flipper;

#ifdef __private_include__

extern void flipper_configure(void);

extern void flipper_attach(char *device, uint8_t source, ...);

#endif

#endif