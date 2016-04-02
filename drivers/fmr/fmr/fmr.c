#define __private_include__

#include <flipper/drivers.h>
#include <flipper/fmr.h>

const void * const objects[] = { &host, &device, &self, &button, &at45, &fs, &io, &led, &pwm, &sam, &spi, &timer, &usart, &usart, &usart, &usb, &wifi, &fmr };

void fmr_configure(void) {

}

fmr_module fmr_bind(char *bundle) {

	/* ~ Generate a handle for the given bundle identifier. ~ */
	fmr_module handle = checksum(bundle, strlen(bundle));

	/* ~ Invoke the remote bind function and obtain the bound handle. ~ */
	host.invoke(_fmr, _fmr_bind, host_args(handle));

	/* ~ Return the handle back to the user. ~ */
	return (fmr_module)(handle);

}

extern uint8_t fmr_padding;

#define FMR_INVOKE_OFFSET 6 * 2

uint32_t fmr_invoke(fmr_module handle, uint8_t index, uint8_t argc, ...) {

	/* ~ Construct a va_list to access variadic arguments. ~ */

	va_list argv;

	/* ~ Initialize the va_list that we created above. ~ */

	va_start(argv, argc);

	/* ~ Load the variadic arguments into the outgoing packet. ~ */

	for (unsigned i = 0; i < (argc * 2); i += 2) {

		/* ~ Unstage an argument from the variadic argument list. ~ */

		unsigned arg = va_arg(argv, unsigned);

		/* ~ Load the argument into the outgoing packet buffer. ~ */

		fmrpacket.body[i + FMR_INVOKE_OFFSET] = hi(arg); fmrpacket.body[i + FMR_INVOKE_OFFSET + 1] = lo(arg);

	}

	/* ~ Release the variadic argument list. ~ */

	va_end(argv);

#pragma message("It would be better to find a way to implement the fmr_padding to not be global.")

	fmr_padding = argc * 2;

	/* ~ Call the remote function and return. ~ */

	return host.invoke(_fmr, _fmr_invoke, fmr_args(handle, index, argc));

}

void *fmr_resolve(void *source, uint32_t length) {

	return NULL;

}
