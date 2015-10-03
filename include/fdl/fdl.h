#ifndef fdl_h

#define fdl_h

#include <flipper/types.h>

extern const struct _fdl {

	void (* configure)(void);

	void (* activate)(char *library, void *location);

} fdl;

#ifdef __private_include__

#endif 

#endif