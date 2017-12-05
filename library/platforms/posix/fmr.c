#define __private_include__
#include <flipper/carbon.h>
#include <flipper/posix/posix.h>

/* Define the standard modules based on platform specific usage declarations. */
const void *const fmr_modules[] = {
	&adc,
	&button,
	&dac,
	&fld,
	&gpio,
	&i2c,
	&led,
	&pwm,
	&rtc,
	&spi,
	&swd,
	&task,
	&temp,
	&timer,
	&uart0,
	&usart,
	&usb,
	&wdt
};

LF_WEAK lf_return_t fmr_call(lf_return_t (* function)(void), fmr_type ret, uint8_t argc, uint16_t argt, void *argv) {
	/* Grab the symbol name of the function for debugging purposes. */
#ifdef __dl_info__
	#include <dlfcn.h>
	Dl_info info;
	dladdr(function, &info);
	printf("Calling local function '%s', with %i arguments, arg types %i, and va_list %p.\n", info.dli_sname, argc, argt, argv);
#endif
	return function();
}

LF_WEAK lf_return_t fmr_push(struct _fmr_push_pull_packet *packet) {
	return -1;
}

LF_WEAK lf_return_t fmr_pull(struct _fmr_push_pull_packet *packet) {
	return -1;
}
