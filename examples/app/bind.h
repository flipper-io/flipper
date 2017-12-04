#ifndef __bind_h__
#define __bind_h__

#include <flipper/libflipper.h>

extern const struct _bind {
	int (* my_func)(uint8_t a);
} bind;

enum { _bind_my_func };

int bind_configure(void);
int my_func(uint8_t a);

#endif
