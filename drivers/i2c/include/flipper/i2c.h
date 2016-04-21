#ifndef __i2c_h__
#define __i2c_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* ~ Declare the virtual driver object. ~ */
extern const struct _i2c {

	void (* configure)(void);
	int (* put)(int mode, int adddress, void *data, int length);
	int (* get)(int mode, int adddress, void *data, int length);

} i2c;

#ifdef __private_include__

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _i2c_configure };

/* ~ Declare all function prototypes for this driver. ~ */
void i2c_configure(void);
int i2c_put(int mode, int address, void *data, int length);
int i2c_get(int mode, int address, void *data, int length);

#endif
#endif
