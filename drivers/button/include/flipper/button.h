#ifndef __button_h__
#define __button_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* ~ Declare the virtual driver object. ~ */
extern const struct _button {

	void (* configure)(void);
	uint8_t (* read)(void);

} button;

#ifdef __private_include__

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _button_configure, _button_read };

/* ~ Declare all function prototypes for this driver. ~ */
void button_configure(void);
uint8_t button_read(void);

#endif
#endif
