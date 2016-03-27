#ifndef __temp_h__
#define __temp_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/flipper/core.h>

/* ~ Declare the virtual driver object. ~ */
extern const struct _temp {

	void (* configure)(void);

} temp;

#ifdef __private_include__

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _temp_configure };

/* ~ Declare all function prototypes for this driver. ~ */
extern void temp_configure(void);

#endif
#endif
