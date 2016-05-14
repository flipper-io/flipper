#ifndef __pwm_h__
#define __pwm_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

/* Declare the virtual interface for this module. */
extern const struct _pwm {

	void (* configure)(void);

} pwm;

#ifdef __private_include__

/* Declare the FMR overlay for this driver. */
enum { _pwm_configure };

/* Declare each prototype for all functions within this driver. */
void pwm_configure(void);

#endif
#endif
