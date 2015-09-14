#ifndef __i2c_h__

#define __i2c_h__

#include <types.h>

enum { _i2c_configure };

extern const struct _i2c {
	
	void (* configure)(void);
	
} i2c;

#ifdef __private_include__

extern void i2c_configure(void);

#endif

#endif