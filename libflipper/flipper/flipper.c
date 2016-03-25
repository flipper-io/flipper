#define __private_include__
#include <flipper/flipper.h>
#include <fmr/fmr.h>
#include <platform.h>
#include <usb/usb.h>
#include <dlfcn.h>
#include <network.h>

struct _flipper flipper = {
	flipper_configure,
	flipper_attach
};

void flipper_configure(void) {

}

void flipper_attach(uint8_t source, ...) {

	/* ~ Construct a va_list to access variadic arguments. ~ */
	va_list argv;

	/* ~ Initialize the va_list that we created above. ~ */
	va_start(argv, source);

	/* ~ Create a pointer to store loaded variadic arguments. ~ */
	char *arg;

	/* ~ Select the source. ~ */
	switch (source) {

		case FLIPPER_SOURCE_USB:

			/* ~ Configure the USB. ~ */
			usb_configure();

			/* ~ Set up the FMR host to accept incoming USB connections. ~ */
			host_configure(&usb);

			/* ~ Set up the FMR device to accept incoming USB connections. ~ */
			device_configure(&usb);

			break;

		case FLIPPER_SOURCE_NETWORK:

			/* ~ Get the IP address. ~ */
			arg = va_arg(argv, char *);

			/* ~ Configure the network. ~ */
			network_configure(arg);

			/* ~ Set up the FMR host to accept incoming network connections. ~ */
			host_configure(&network);

			/* ~ Set up the FMR device to accept incoming network connections. ~ */
			device_configure(&network);

			break;

		case FLIPPER_SOURCE_FVM:

			/* ~ Get the FVM path from the va_list. ~ */
			arg = va_arg(argv, char *);

			/* ~ Load the FVM. ~ */
			void *vm = dlopen(arg, RTLD_LAZY);

			/* ~ Ensure that the VM was loaded successfully. ~ */
			if (!vm) {
				error.raise(E_FVM_LOAD, ERROR_STRING(E_FVM_LOAD_S));
				break;
			}

			/* ~ Get the address of the debug bus. ~ */
			const struct _bus *fdb = dlsym(vm, "fdb");

			if (dlerror() != NULL) {
				error.raise(E_FVM_SYM, ERROR_STRING(E_FVM_SYM_S));
				break;
			}

			/* ~ Get the address of the configuration function. ~ */
			void (* fvm_configure)(const struct _bus *) = dlsym(vm, "fvm_configure");

			if (dlerror() != NULL) {
				error.raise(E_FVM_SYM, ERROR_STRING(E_FVM_SYM_S));
				break;
			}

			/* ~ Configure FVM to use the FDB communications channel. ~ */
			fvm_configure(fdb);

			/* ~ Get the address of the virtual target. ~ */
			const struct _target *fvm = dlsym(vm, "fvm");

			if (dlerror() != NULL) {
				error.raise(E_FVM_SYM, ERROR_STRING(E_FVM_SYM_S));
				break;
			}

			/* Redirect the host and the device to FVM. */
			memcpy((void *)(&host), (void *)(fvm), sizeof(struct _target));
			memcpy((void *)(&device), (void *)(fvm), sizeof(struct _target));

			break;

		default:

			break;

	}

	/* ~ Release the va_list. ~ */
	va_end(argv);

}