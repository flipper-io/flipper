#ifndef __app_h__
#define __app_h__

#include <flipper.h>

extern const struct _app {
	int (* my_func)(uint8_t a);
} app;

enum { _app_my_func };

int app_configure(void);
int my_func(uint8_t a);

#endif
