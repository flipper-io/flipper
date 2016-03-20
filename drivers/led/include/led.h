#ifndef __led_h__
#define __led_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* ~ Declare all public macros for this driver. ~ */
#define LED_OFF 0, 0, 0
#define LED_COLOR_BUSY 25, 0, 0
#define LED_COLOR_SUCCESS 0, 25, 0
#define LED_COLOR_ERROR 25, 0, 0

/* ~ Declare the virtual driver object. ~ */
extern const struct _led {

	void (* configure)(void);
	void (* rgb)(uint8_t r, uint8_t g, uint8_t b);

} led;

#ifdef __private_include__

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _led_configure, _led_set_rgb };

/* ~ Declare all function prototypes for this driver. ~ */
extern void led_configure(void);
extern void led_set_rgb(uint8_t r, uint8_t g, uint8_t b);

#endif
#endif
