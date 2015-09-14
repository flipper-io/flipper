#define __private_include__

#include <flipper.h>

#include <fmr/fmr.h>

#include <usb/usb.h>

#include <network/network.h>

void flipper_configure(void) {
	
	
	
}

void flipper_attach(uint8_t source, ...) {
	
	/* ~ Construct a va_list to access varidic arguments. ~ */
	
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
			
		default:
			
			break;
			
	}
	
}