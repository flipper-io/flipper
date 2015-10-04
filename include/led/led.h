#ifndef __led_h__

#define __led_h__

#include <flipper/types.h>

#define LED_OFF 0, 0, 0

#define LED_COLOR_BUSY 25, 0, 0

#define LED_COLOR_SUCCESS 0, 25, 0

#define LED_COLOR_ERROR 25, 0, 0

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