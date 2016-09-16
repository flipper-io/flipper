#include <stdio.h>
#define __private_include__
#include <flipper.h>
#include <platform/posix.h>
#include <platform/fvm.h>

int main(int argc, char *argv[]) {

#if 0

	flipper_attach();
	uint8_t packet[FMR_PACKET_SIZE];
	packet[0] = atoi(argv[1]);
	packet[1] = atoi(argv[2]);
	packet[2] = atoi(argv[3]);
	libusb_push(packet, sizeof(packet));

#else

	//flipper_attach_endpoint("fvm", &lf_fvm_ep);

	flipper_attach();

	/* Create an empty message runtime module instance. */
	struct _fmr_module _lf_led_module;

	/* Bind the empty instance to the default 'led' module which comes pre-loaded on all devices. */
	int _e = fmr_bind(&_lf_led_module, "_lf_led");
	if (_e < lf_success) {
		error_raise(E_MODULE, "Failed to bind to LED module.");
	}

	/* Perform the function invocation. */
	lf_invoke(&_lf_led_module, _led_set_rgb, fmr_args(fmr_int8(atoi(argv[1])), fmr_int8(atoi(argv[2])), fmr_int8(atoi(argv[3]))));

#endif

    return 0;
}
