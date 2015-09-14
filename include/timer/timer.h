#ifndef __timer_h__

#define __timer_h__

#include <types.h>

enum { _timer_configure };

extern const struct _timer {
	
	void (* configure)(void);
	
} timer;

#ifdef __private_include__

extern void timer_configure(void);

#endif

#endif