#define __private_include__

#include <libflipper.h>

#include <fmr/fmr.h>

#include <fvm/fvm.h>

#include <usb/usb.h>

#include <network/network.h>

const struct _flipper flipper = {
	
	flipper_configure,
	
	flipper_attach
	
};

void flipper_configure(void) {
	
	
	
}

void flipper_attach(uint8_t source, ...) {
	
	/* ~ Construct a va_list to access variadic arguments. ~ */
	
	va_list argv;
	
	/* ~ Select the source. ~ */
	
	switch (source) {
			
		case FLIPPER_SOURCE_USB:
			
			/* ~ Configure the USB. ~ */
			
			usb_configure(0);
			
			/* ~ Set up the FMR host to accept incoming USB connections. ~ */
			
			host_configure(&usb);
			
			/* ~ Set up the FMR device to accept incoming USB connections. ~ */
			
			device_configure(&usb);
			
			break;
			
		case FLIPPER_SOURCE_NETWORK:
			
			/* ~ Initialize the va_list that we created above. ~ */
			
			va_start(argv, source);
			
			/* ~ Get the network address from the va_list. ~ */
			
			char *ip = va_arg(argv, char *);
			
			/* ~ Configure the network. ~ */
			
			network_configure(ip);
			
			/* ~ Set up the FMR host to accept incoming network connections. ~ */
			
			host_configure(&network);
			
			/* ~ Set up the FMR device to accept incoming network connections. ~ */
			
			device_configure(&network);
			
			/* ~ Release the va_list. ~ */
			
			va_end(argv);
			
			break;
			
		case FLIPPER_SOURCE_FVM:
			
			verbose("Successfully attatched to FVM.\n\n");
			
			/* Redirect the host and the device to FVM. */
			
			memcpy((void *)(&host), (void *)(&fvm), sizeof(struct _target));
			
			memcpy((void *)(&device), (void *)(&fvm), sizeof(struct _target));
			
			/* ~ Configure FVM to use the FDB communications channel. ~ */
			
			fvm_configure(&fdb);
			
			break;
			
		default:
			
			break;
			
	}
	
}