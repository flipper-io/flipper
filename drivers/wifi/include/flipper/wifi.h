#ifndef __wifi_h__
#define __wifi_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

/* Declare the virtual interface for this module. */
extern const struct _wifi {

	void (* configure)(void);

} wifi;

#ifdef __private_include__

/* Declare the FMR overlay for this driver. */
enum { _wifi_configure };

/* Declare each prototype for all functions within this driver. */
void wifi_configure(void);

#endif
#endif
