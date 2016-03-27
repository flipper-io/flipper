#ifndef __wifi_h__
#define __wifi_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/flipper/core.h>

/* ~ Declare the virtual driver object. ~ */
extern const struct _wifi {

	void (* configure)(void);

} wifi;

#ifdef __private_include__

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _wifi_configure };

/* ~ Declare all function prototypes for this driver. ~ */
extern void wifi_configure(void);

#endif
#endif
