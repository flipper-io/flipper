#ifndef __template_h__
#define __template_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* ~ Declare the virtual driver object. ~ */
extern const struct _template {

	void (* configure)(void);

} template;

#ifdef __private_include__

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _template_configure };

/* ~ Declare all function prototypes for this driver. ~ */
extern void template_configure(void);

#endif
#endif
