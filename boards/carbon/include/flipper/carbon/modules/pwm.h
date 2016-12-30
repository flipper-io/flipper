#ifndef __pwm_h__
#define __pwm_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

/* Declare the virtual interface for this module. */
extern const struct _pwm {

	int (* configure)(void);

} pwm;

#ifdef __private_include__

/* The fmr_module structure for this module. */
extern struct _lf_module _pwm;

/* Declare the FMR overlay for this driver. */
enum { _pwm_configure };

/* Declare each prototype for all functions within this driver. */
int pwm_configure(void);

#endif
#endif
