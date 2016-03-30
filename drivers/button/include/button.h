#ifndef __button_h__
#define __button_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* ~ Declare the virtual driver object. ~ */
extern const struct _button {

	void (* configure)(void);
	bool (* read)(void);

} button;

#ifdef __private_include__

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _button_configure, _button_read };

/* ~ Declare all function prototypes for this driver. ~ */
extern void button_configure(void);
extern bool button_read(void);

#endif
#endif
