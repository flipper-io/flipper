#ifndef __button_h__

#define __button_h__

#include <flipper/types.h>

extern const struct _button {
	
	void (* configure)(void);
	
	bool (* read)(void);
	
} button;

#ifdef __private_include__

enum { _button_configure, _button_read };

extern void button_configure(void);

extern bool button_read(void);

#endif

#endif