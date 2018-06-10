#include <flipper.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <dlfcn.h>
#include <flipper/posix/network.h>

/* fvm - Creates a local server that acts as a virtual flipper device. */

int main(int argc, char *argv[]) {

	struct sockaddr_in addr;
	int sd;
	int e;
	struct _lf_network_context *context;
	struct _lf_device *fvm = NULL;

	sd = socket(PF_INET, SOCK_DGRAM, IPPROTO_UDP);
	lf_assert(sd > 0, E_ENDPOINT, "Failed to obtain socket.");

	bzero(&addr, sizeof(addr));
	addr.sin_family = AF_INET;
	addr.sin_port = htons(LF_UDP_PORT);
	addr.sin_addr.s_addr = htonl(INADDR_ANY);

	e = bind(sd, (struct sockaddr*)&addr, sizeof(addr));
	lf_assert(e == 0, E_ENDPOINT, "Failed to create server.");

	/* The network endpoint for the virtual flipper device. */
	fvm = lf_device_create(lf_network_read, lf_network_write, lf_network_release);
	lf_assert(fvm, E_ENDPOINT, "Failed to create device for virtual machine.");

	fvm->_ep_ctx = calloc(1, sizeof(struct _lf_network_context));
	context = (struct _lf_network_context *)fvm->_ep_ctx;
	lf_assert(context, E_NULL, "Failed to allocate memory for context");

	context->fd = sd;

	lf_attach(fvm);

	printf("Flipper Virtual Machine (FVM) v0.1.0\nListening on 'localhost'.\n\n");

	extern struct _lf_module adc;
	extern struct _lf_module button;
	extern struct _lf_module dac;
	extern struct _lf_module gpio;
	extern struct _lf_module i2c;
	extern struct _lf_module led;
	extern struct _lf_module pwm;
	extern struct _lf_module rtc;
	extern struct _lf_module spi;
	extern struct _lf_module swd;
	extern struct _lf_module temp;
	extern struct _lf_module timer;
	extern struct _lf_module uart0;
	extern struct _lf_module usart;
	extern struct _lf_module usb;
	extern struct _lf_module wdt;

	dyld_register(fvm, &adc);
	dyld_register(fvm, &button);
	dyld_register(fvm, &dac);
	dyld_register(fvm, &gpio);
	dyld_register(fvm, &i2c);
	dyld_register(fvm, &led);
	dyld_register(fvm, &pwm);
	dyld_register(fvm, &rtc);
	dyld_register(fvm, &spi);
	dyld_register(fvm, &swd);
	dyld_register(fvm, &temp);
	dyld_register(fvm, &timer);
	dyld_register(fvm, &uart0);
	dyld_register(fvm, &usart);
	dyld_register(fvm, &usb);
	dyld_register(fvm, &wdt);

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

			e = dyld_register(fvm, m);
			lf_assert(e , E_NULL, "Failed to register module '%s'.", m->name);
			printf("Successfully registered module '%s'.", module);
		}

		printf("\n");
	}

	struct _fmr_packet packet;

	while (1) {
		fvm->read(fvm, &packet, sizeof(struct _fmr_packet));
		lf_debug_packet(&packet, sizeof(struct _fmr_packet));
		fmr_perform(fvm, &packet);
	}

	close(sd);

fail:
	return EXIT_FAILURE;
}
