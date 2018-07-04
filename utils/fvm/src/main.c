#include "libflipper.h"
#include <unistd.h>
#include <arpa/inet.h>
#define _GNU_SOURCE
#include <dlfcn.h>
#include "posix/network.h"

/* fvm - Creates a local server that acts as a virtual flipper device. */

struct _lf_device *fvm = NULL;

int main(int argc, char *argv[]) {

	//lf_set_debug_level(LF_DEBUG_LEVEL_ALL);

	/* Create a UDP server. */
	struct sockaddr_in addr;
	int sd = socket(PF_INET, SOCK_DGRAM, IPPROTO_UDP);
	if (sd < 0) {
		printf("Failed to get socket.\n");
		return 0;
	}
	bzero(&addr, sizeof(addr));
	addr.sin_family = AF_INET;
	addr.sin_port = htons(LF_UDP_PORT);
	addr.sin_addr.s_addr = htonl(INADDR_ANY);
	int e = bind(sd, (struct sockaddr*)&addr, sizeof(addr));
	if (e < 0) {
		printf("Failed to create server.\n");
		return 0;
	}

	/* The network endpoint for the virtual flipper device. */
	fvm = lf_device_create(lf_network_read, lf_network_write, lf_network_release);
	lf_assert(fvm, E_ENDPOINT, "Failed to create device for virtual machine.");
	fvm->_ep_ctx = calloc(1, sizeof(struct _lf_network_context));
	struct _lf_network_context *context = (struct _lf_network_context *)fvm->_ep_ctx;
	lf_assert(context, E_NULL, "Failed to allocate memory for context");
	/* Set server file descriptor. */
	context->fd = sd;
	lf_attach(fvm);

	printf("Flipper Virtual Machine (FVM) v0.1.0\nListening on 'localhost'.\n\n");

    extern struct _lf_module adc;
	dyld_register(fvm, &adc);
    adc_configure();

    extern struct _lf_module button;
	dyld_register(fvm, &button);
    button_configure();

    extern struct _lf_module dac;
	dyld_register(fvm, &dac);
    dac_configure();

    extern struct _lf_module gpio;
	dyld_register(fvm, &gpio);
    gpio_configure();

    extern struct _lf_module i2c;
	dyld_register(fvm, &i2c);
    i2c_configure();

    extern struct _lf_module led;
	dyld_register(fvm, &led);
    led_configure();

    extern struct _lf_module pwm;
	dyld_register(fvm, &pwm);
    pwm_configure();

    extern struct _lf_module rtc;
	dyld_register(fvm, &rtc);
    rtc_configure();

    extern struct _lf_module spi;
	dyld_register(fvm, &spi);
    spi_configure();

    extern struct _lf_module swd;
	dyld_register(fvm, &swd);
    swd_configure();

    extern struct _lf_module temp;
	dyld_register(fvm, &temp);
    temp_configure();

    extern struct _lf_module timer;
	dyld_register(fvm, &timer);
    timer_configure();

    extern struct _lf_module uart0;
	dyld_register(fvm, &uart0);
    uart0_configure();

    extern struct _lf_module usart;
	dyld_register(fvm, &usart);
    usart_configure();

    extern struct _lf_module usb;
	dyld_register(fvm, &usb);
    usb_configure();

    extern struct _lf_module wdt;
	dyld_register(fvm, &wdt);
	wdt_configure();

	if (argc > 1) {
		char *lib = argv[1];
		char *module, **modules = &argv[2];
		while ((module = *modules++)) {
			printf("Loading module '%s' from '%s'.", module, lib);
			void *dlm = dlopen(lib, RTLD_LAZY);
			lf_assert(dlm, E_NULL, "Failed to open module '%s'.", lib);
			struct _lf_module *m = dlsym(dlm, module);
			lf_assert(m, E_NULL, "Failed to read module '%s' from '%s'.", module, lib);
			printf("Successfully loaded module '%s'.", module);
			int e = dyld_register(fvm, m);
			lf_assert(e , E_NULL, "Failed to register module '%s'.", m->name);
			printf("Successfully registered module '%s'.", module);
		}
		printf("\n");
	}

	while (1) {
		struct _fmr_packet packet;
		fvm->read(fvm, &packet, sizeof(struct _fmr_packet));
		lf_debug_packet(&packet, sizeof(struct _fmr_packet));
		lf_error_set(E_OK);
		fmr_perform(fvm, &packet);
	}

	close(sd);

fail:
	return EXIT_FAILURE;
}
