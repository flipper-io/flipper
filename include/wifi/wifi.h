#ifndef __wifi_h__

#define __wifi_h__

#include <flipper/types.h>

extern const struct _wifi {
	
	void (* configure)(void);
	
	uint32_t (* ip)(void);
	
} wifi;

#ifdef __private_include__

enum { _wifi_configure, _wifi_ip };

extern void wifi_configure(void);

extern uint32_t wifi_ip(void);

#endif

#endif