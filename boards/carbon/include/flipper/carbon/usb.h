#ifndef __usb_h__
#define __usb_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

/* Declare the virtual interface for this module. */
extern const struct _usb {
	int (* configure)(void);
} usb;

#ifdef __private_include__

/* The fmr_module structure for this module. */
extern struct _lf_module _usb;

/* Declare the FMR overlay for this driver. */
enum { _usb_configure };

/* Declare each prototype for all functions within this driver. */
int usb_configure(void);

#endif
#endif
