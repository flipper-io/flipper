#ifndef __adc_h__
#define __adc_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/libflipper.h>

/* Declare the virtual interface for this module. */
extern const struct _adc {
	int (* configure)(void);
} adc;

#ifdef __private_include__

/* The fmr_module structure for this module. */
extern struct _lf_module _adc;

/* Declare the FMR overlay for this driver. */
enum { _adc_configure };

/* Declare each prototype for all functions within this driver. */
int adc_configure(void);

#endif
#endif
