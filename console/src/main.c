#include <stdio.h>
#define __private_include__
#include <flipper.h>
#include <platform/posix.h>
#include <platform/fvm.h>

int main(int argc, char *argv[]) {

	flipper_attach_endpoint("fvm", &lf_fvm_ep);
//	flipper_attach();

	adc.configure();
	printf("%p\n", _adc);
	printf("%s: %s\n", _adc -> name, _adc -> description);

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
	lf_invoke(&_lf_led_module, _lf_led_set_rgb, fmr_args(fmr_int8(0), fmr_int8(1), fmr_int8(2)));

    return 0;
}
