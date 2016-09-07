#include <stdio.h>
#define __private_include__
#include <flipper/flipper.h>
#include <flipper/fmr.h>
#include <platform/fvm.h>

int main(int argc, char *argv[]) {

	fvm_begin();
	flipper_attach_endpoint("fvm", &lf_fvm_ep);

	int red = 1, green = 2, blue = 3;

	/* A simple explaination of modules using the default 'led' module. */

	/* Create an empty message runtime module instance. */
	struct _fmr_module _lf_led_module;
	/* Bind the empty instance to the default 'led' module which comes pre-loaded on all devices. */
	int _e = fmr_bind(&_lf_led_module, "_lf_led");
	if (_e < lf_success) {
		error_raise(E_MODULE, "Failed to bind to LED module.");
	}
	/* Define the enumerator that will be laid over the driver structure. */
	enum { _lf_led_configure, _lf_led_set_rgb };
	/* Perform the function invocation. */
	lf_invoke(&_lf_led_module, _lf_led_set_rgb, fmr_args(fmr_int8(red), fmr_int8(green), fmr_int8(blue)));

	flipper_exit();

    return 0;
}
