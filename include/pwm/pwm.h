#ifndef __pwm_h__

#define __pwm_h__

#include <types.h>

extern const struct _pwm {
	
	void (* configure)(void);
	
} pwm;

#ifdef __private_include__

enum { _pwm_configure };

void pwm_configure(void);

#endif

#endif