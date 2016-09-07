#ifndef __button_h__
#define __button_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

/* Declare the virtual interface for this module. */
extern const struct _button {
	/* Configures the button hardware. */
	void (* configure)(void);
	/* Reads back the button state; returns 0 when released and 1 when pressed. */
	uint8_t (* read)(void);
} button;

#ifdef __private_include__

/* Declare the FMR overlay for this driver. */
enum { _button_configure, _button_read };

/* Declare each prototype for all functions within this driver. */
void button_configure(void);
uint8_t button_read(void);

#endif
#endif
