#ifndef __pwm_h__
#define __pwm_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* ~ Declare the virtual driver object. ~ */
extern const struct _pwm {

	void (* configure)(void);

} pwm;

#ifdef __private_include__

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _pwm_configure };

/* ~ Declare all function prototypes for this driver. ~ */
extern void pwm_configure(void);

#endif
#endif
