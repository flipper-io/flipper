#ifndef __sam_h__
#define __sam_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* ~ Declare the virtual driver object. ~ */
extern const struct _sam {

	void (* configure)(void);
	void (* power)(bool power);
	void (* reset)(void);
	void (* dfu)(void);
	void (* format)(void);
	void (* suspend)(void);
	void (* engage)(void);

} sam;

#ifdef __private_include__

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _sam_configure, _sam_set_power, _sam_reset, _sam_load_dfu, _sam_format, _sam_suspend, _sam_engage };

/* ~ Declare all function prototypes for this driver. ~ */
extern void sam_configure(void);
extern void sam_set_power(bool power);
extern void sam_reset(void);
extern void sam_load_dfu(void);
extern void sam_format(void);
extern void sam_suspend(void);
extern void sam_engage(void);

#endif
#endif
