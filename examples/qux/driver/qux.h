#ifndef qux_h

#define qux_h

#include <flipper/types.h>

#include <fmr/fmr.h>

/* ~ Declare the virtual interface for the qux driver. ~ */

extern const struct _qux {
	
	void (* configure)(void);
	
	void (* on)(void);
	
	void (* off)(void);
	
} qux;

/* ~ Define the private definitions of the driver functions. ~ */

#ifdef __private_include__

/* ~ Declare the enum overlay for this driver. ~ */

enum { _qux_configure, _qux_on, _qux_off };

/* ~ Configures the necessary IO for the interface. ~ */

extern void qux_configure(void);

extern void qux_on(void);

extern void qux_off(void);

#endif

#endif