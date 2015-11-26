#ifndef fdl_h

#define fdl_h

#include <flipper/types.h>

extern const struct _fdl {

	void (* configure)(void);

	void (* activate)(uint16_t key);

} fdl;

#ifdef __private_include__

enum { _fdl_configure, _fdl_activate };

void fdl_configure(void);

void fdl_activate(uint16_t key);

#endif 

#endif