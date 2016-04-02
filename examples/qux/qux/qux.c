#define __private_include__
#include "qux.h"

/* ~ Create a handle for the module. ~ */
static fmr_module handle;

void qux_configure(void) {

	/* ~ Registers this module with the Flipper Message Runtime. Also calls the remote configure function. ~ */
	handle = fmr.bind("io.flipper.qux");

}

void qux_on(void) {

	/* ~ Invokes the function 'qux_on' on the device. ~ */
	fmr.invoke(handle, _qux_on, NO_ARGS);

}

void qux_off(void) {

	/* ~ Invokes the function 'qux_off' on the device. ~ */
	fmr.invoke(handle, _qux_off, NO_ARGS);

}