#ifndef __led_h__

#define __led_h__

#include <flipper/types.h>

extern const struct _led {
	
	void (* configure)(void);
	
	void (* rgb)(uint8_t r, uint8_t g, uint8_t b);
	
} led;

#ifdef __private_include__

enum { _led_configure, _led_set_rgb };

void led_configure(void);

void led_set_rgb(uint8_t r, uint8_t g, uint8_t b);

#endif

#endif