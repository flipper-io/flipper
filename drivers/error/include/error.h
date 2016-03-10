#ifndef __error_h__
#define __error_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* ~ Declare the virtual driver object. ~ */
extern const struct _error {

	void (* configure)(void);
	void (* raise)(uint16_t id);
	char *(* message)(void);

	uint16_t error;

} error;

#ifdef __private_include__

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _error_configure, _error_raise, _error_message };

/* ~ Declare all function prototypes for this driver. ~ */
extern void error_configure(void);
extern void error_raise(uint16_t id);
extern char *error_message(void);

#endif
#endif