#ifndef __pwm_h__
#define __pwm_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper.h>

/* Declare the virtual interface for this module. */
extern const struct _pwm_interface {

	int (* configure)(void);

} pwm;

/* Declare the _lf_module structure for this module. */
extern struct _lf_module _pwm;

/* Declare the FMR overlay for this module. */
enum { _pwm_configure };

/* Declare the prototypes for all of the functions within this module. */
int pwm_configure(void);

#endif
