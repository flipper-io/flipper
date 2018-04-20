#ifndef __button_h__
#define __button_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper.h>

/* Declare the virtual interface for this module. */
extern const struct _button_interface {
	/* Configures the button hardware. */
	int (* configure)(void);
	/* Reads back the button state; returns 0 when released and 1 when pressed. */
	uint8_t (* read)(void);
} button;

/* Declare the _lf_module structure for this module. */
extern struct _lf_module _button;

/* Declare the FMR overlay for this module. */
enum { _button_configure, _button_read };

/* Declare the prototypes for all of the functions within this module. */
int button_configure(void);
uint8_t button_read(void);

#endif
