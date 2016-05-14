#ifndef __template_h__
#define __template_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

/* Declare the virtual interface for this module. */
extern const struct _template {

	void (* configure)(void);

} template;

#ifdef __private_include__

/* Declare the FMR overlay for this driver. */
enum { _template_configure };

/* Declare each prototype for all functions within this driver. */
extern void template_configure(void);

#endif
#endif
