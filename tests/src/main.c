#include <stdio.h>
#define __private_include__
#include <flipper.h>
#include <platform/posix.h>
#include <platform/fvm.h>

#define v(i) atoi(argv[i])

int main(int argc, char *argv[]) {

#if 1

	flipper_attach();

	/* Bulk endpoint tests. */
	char buffer[FMR_PACKET_SIZE];
	buffer[0] = v(1);
	buffer[1] = v(2);
	buffer[2] = v(3);
	libusb_transfer(buffer, FMR_PACKET_SIZE, 0x01);
	libusb_transfer(buffer, FMR_PACKET_SIZE, 0x80);
	printf("0x%02x 0x%02x 0x%02x\n", buffer[0], buffer[1], buffer[2]);

#else

	flipper_attach_endpoint("fvm", &lf_fvm_ep);
	flipper_attach();

	if (argc < 4) {
		error_raise(E_NULL, "Invalid number of arguments provided. Expected R, G, B.");
		return EXIT_FAILURE;
	}

	/* Create an empty message runtime module instance. */
	struct _fmr_module _lf_led_module;
	_lf_led_module.device = flipper.device;

	/* Bind the empty instance to the default 'led' module which comes pre-loaded on all devices. */
	int _e = fmr_bind(&_lf_led_module, "_lf_led");
	if (_e < lf_success) {
		error_raise(E_MODULE, "Failed to bind to LED module.");
	}
	_lf_led_module.identifier = 0;

	/* Perform the function invocation. */
	uint32_t result = lf_invoke(&_lf_led_module, _led_set_rgb, fmr_args(fmr_int8(atoi(argv[1])), fmr_int8(atoi(argv[2])), fmr_int8(atoi(argv[3]))));
	printf("The value was 0x%08x.\n", result);

#endif

    return 0;
}
