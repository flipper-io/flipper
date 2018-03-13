#include <flipper.h>
#include <unistd.h>
#include <arpa/inet.h>
#define _GNU_SOURCE
#include <dlfcn.h>
#include <flipper/posix/network.h>

/* fvm - Creates a local server that acts as a virtual flipper device. */

#undef lf_debug
#define lf_debug(format, ...) printf(format"\n", __VA_ARGS__)

int fvm_load_module(char *path, char *module) {
	void *dlm = dlopen(path, RTLD_LAZY);
	lf_assert(dlm, failure, E_NULL, "Failed to open module '%s'.", path);
	struct _lf_module *m = dlsym(dlm, module);
	lf_assert(m, failure, E_NULL, "Failed to read module '%s' from '%s'.", m->name, path);
	dyld_register(&THIS_DEVICE, m);
	lf_debug("Successfully loaded module '%s'.", module);
	return lf_success;
failure:
	return lf_error;
}

struct _lf_endpoint *nep = NULL;

/* Modules. */
extern struct _lf_module adc;
extern struct _lf_module button;
extern struct _lf_module dac;
extern struct _lf_module fld;
extern struct _lf_module fs;
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

int main(int argc, char *argv[]) {

	//lf_set_debug_level(LF_DEBUG_LEVEL_ALL);

	if (argc > 1) {
		char *lib = argv[1];
		char **modules = &argv[2];
		for (int i = 0; i < (argc-2); i ++) {
			lf_debug("Loading module '%s' from '%s'.", *modules, lib);
			fvm_load_module(lib, *modules++);
		}
	}

	THIS_DEVICE.name = "fvm";

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
	int _e = bind(sd, (struct sockaddr*)&addr, sizeof(addr));
	if (_e < 0) {
		printf("Failed to create server.\n");
		return 0;
	}

	/* The network endpoint for the virtual flipper device. */
	struct _lf_network_context *context = NULL;
	nep = lf_endpoint_create(lf_network_configure,
							 lf_network_ready,
							 lf_network_push,
							 lf_network_pull,
							 lf_network_destroy,
							 sizeof(struct _lf_network_context));
	lf_assert(nep, failure, E_ENDPOINT, "Failed to create endpoint for networked device.");
	context = (struct _lf_network_context *)nep->_ctx;
	context->fd = sd;

	printf("Flipper Virtual Machine (FVM) v0.1.0\nListening on 'localhost'.\n\n");

	/* Register the modules. */
	dyld_register(&THIS_DEVICE, &adc);
	dyld_register(&THIS_DEVICE, &button);
	dyld_register(&THIS_DEVICE, &dac);
//	dyld_register(&THIS_DEVICE, &fld);
//	dyld_register(&THIS_DEVICE, &fs);
	dyld_register(&THIS_DEVICE, &gpio);
	dyld_register(&THIS_DEVICE, &i2c);
	dyld_register(&THIS_DEVICE, &led);
	dyld_register(&THIS_DEVICE, &pwm);
	dyld_register(&THIS_DEVICE, &rtc);
	dyld_register(&THIS_DEVICE, &spi);
	dyld_register(&THIS_DEVICE, &swd);
	dyld_register(&THIS_DEVICE, &temp);
	dyld_register(&THIS_DEVICE, &timer);
	dyld_register(&THIS_DEVICE, &uart0);
	dyld_register(&THIS_DEVICE, &usart);
	dyld_register(&THIS_DEVICE, &usb);
	dyld_register(&THIS_DEVICE, &wdt);

	while (1) {
		struct _fmr_packet packet;
		nep->pull(nep, &packet, sizeof(struct _fmr_packet));
		lf_debug_packet(&packet, sizeof(struct _fmr_packet));
		struct _fmr_result result;
		lf_error_clear();
		fmr_perform(&packet, &result);
		lf_debug_result(&result);
		nep->push(nep, &result, sizeof(struct _fmr_result));
	}

	close(sd);

failure:
	return EXIT_FAILURE;
}

lf_return_t fmr_push(struct _fmr_push_pull_packet *packet) {
	int retval;
	void *swap = malloc(packet->length);
	lf_assert(swap, failure, E_MALLOC, "Failed to allocate push buffer");
	nep->pull(nep, swap, packet->length);
	*(uint64_t *)(packet->call.parameters) = (uintptr_t)swap;
	retval = fmr_execute(packet->call.index, packet->call.function, packet->call.ret, packet->call.argc, packet->call.types, (void *)(packet->call.parameters));
	free(swap);
	return retval;
failure:
	return lf_error;
}

lf_return_t fmr_pull(struct _fmr_push_pull_packet *packet) {
	lf_return_t retval;
	void *swap = malloc(packet->length);
	lf_assert(swap, failure, E_MALLOC, "Failed to allocate pull buffer");
	*(uint64_t *)(packet->call.parameters) = (uintptr_t)swap;
	retval = fmr_execute(packet->call.index, packet->call.function, packet->call.ret, packet->call.argc, packet->call.types, (void *)(packet->call.parameters));
	nep->push(nep, swap, packet->length);
	free(swap);
	return retval;
failure:
	return lf_error;
}
