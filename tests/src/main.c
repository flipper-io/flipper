#include <stdio.h>
#define __private_include__
#include <flipper.h>
#include <platform/posix.h>
#include <platform/fvm.h>

#define v(i) atoi(argv[i])

void led_test(int argc, char *argv[]);

struct _fmr_module *make_module(char *name, fmr_module identifier) {
	/* Create an empty message runtime module instance. */
	struct _fmr_module *_module = malloc(sizeof(struct _fmr_module));
	if (!_module) {
		error_raise(E_MALLOC, "Malloc error for module.");
		return NULL;
	}
	_module -> device = lf_device();
	/* Bind the empty instance to the default 'led' module which comes pre-loaded on all devices. */
	int _e = fmr_bind(_module, name);
	if (_e < lf_success) {
		error_raise(E_MODULE, error_message("Failed to bind to '%s' module.", name));
		free(_module);
		return NULL;
	}
	_module -> identifier = identifier;
	return _module;
}

int main(int argc, char *argv[]) {

#if 0

	flipper_attach();

	/* Bulk endpoint tests. */
	char buffer[FMR_PACKET_SIZE];
	buffer[0] = v(1);
	buffer[1] = v(2);
	buffer[2] = v(3);
	lf_usb_transfer(buffer, FMR_PACKET_SIZE, 0x01);
	lf_usb_transfer(buffer, FMR_PACKET_SIZE, 0x80);
	printf("0x%02x 0x%02x 0x%02x\n", buffer[0], buffer[1], buffer[2]);

#else

	//flipper_attach_endpoint("fvm", &lf_fvm_ep);
	flipper_attach();

	//led_test(argc, argv);

	/* - FMR Tests - */

	struct _fmr_module *_uart = make_module("uart", _uart_id);

	//uint32_t result = lf_invoke(&_lf_fmr_module, _fmr_push, NULL);
	//char out[] = "#";
	//void *address = lf_push(_uart, _uart_push, out, sizeof(out));
	//printf("Pushed to device. Address is %p.\n", address);
	lf_invoke(_uart, _uart_put, fmr_args(fmr_int8('#')));

	char in[3];
	lf_pull(_uart, _uart_pull, in, sizeof(in));
	printf("Pulled from device. Value is: \n ");
	for (int i = 0; i < sizeof(in); i ++) {
		printf("0x%02x, ", in[i]);
	}
	printf("\n");

#endif

    return 0;
}

void led_test(int argc, char *argv[]) {

	/* Load the device's configuration. */
	printf("connected:\n name: '%s'\n identifier: 0x%04x\n version: 0x%04x\n attributes: 0x%02x\n",
			lf_device() -> configuration.name,
			lf_device() -> configuration.identifier,
			lf_device() -> configuration.version,
			lf_device() -> configuration.attributes);

	if (argc < 4) {
		error_raise(E_NULL, "Invalid number of arguments provided. Expected R, G, B.");
		exit(EXIT_FAILURE);
	}

	/* Create an empty message runtime module instance. */
	struct _fmr_module _lf_led_module;
	_lf_led_module.device = lf_device();

	/* Bind the empty instance to the default 'led' module which comes pre-loaded on all devices. */
	int _e = fmr_bind(&_lf_led_module, "_lf_led");
	if (_e < lf_success) {
		error_raise(E_MODULE, "Failed to bind to LED module.");
	}
	_lf_led_module.identifier = _led_id;

	/* Perform the function invocation. */
	uint32_t result = lf_invoke(&_lf_led_module, _led_set_rgb, fmr_args(fmr_int8(atoi(argv[1])), fmr_int8(atoi(argv[2])), fmr_int8(atoi(argv[3]))));
	printf("The value was 0x%08x.\n", result);

}
