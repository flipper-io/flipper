#ifndef __led_h__
#define __led_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper.h>

/* Declare all public macros for this driver. */
#define LED_OFF 0, 0, 0
#define LED_COLOR_BUSY 25, 0, 0
#define LED_COLOR_SUCCESS 0, 25, 0
#define LED_COLOR_ERROR 25, 0, 0

#define LED_GREEN 0, 10, 0

/* Declare the prototypes for all of the functions within this module. */
int led_configure(void);
void led_rgb(uint8_t r, uint8_t g, uint8_t b);

#endif
