#ifndef __usb_h__
#define __usb_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

/* Declare the virtual interface for this module. */
extern const struct _usb {
	void (* configure)(void);
} usb;

#ifdef __private_include__

/* Declare the FMR overlay for this driver. */
enum { _usb_configure };

/* Declare each prototype for all functions within this driver. */
void usb_configure(void);

#endif
#endif
