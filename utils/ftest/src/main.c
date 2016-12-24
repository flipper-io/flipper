#define __private_include__
#include <flipper.h>
#include <platform/posix.h>
#include <platform/fvm.h>

int main(int argc, char *argv[]) {

	flipper.attach();
	//flipper_attach_endpoint("fvm", &lf_fvm_ep);

	led.rgb(0, 0, 100);

#if 0
	/* Create the SAM4S endpoint using uart0 as the interface. */
	struct _lf_endpoint sam4s_ep = {
		uart0_configure,
		uart0_ready,
		uart0_put,
		uart0_get,
		uart0_push,
		uart0_pull
	};
	/* Create the SAM4S device. */
	struct _lf_device sam4s;
	strcpy(sam4s.configuration.name, "sam4s");
	sam4s.configuration.attributes = lf_device_little_endian | lf_device_32bit;
	sam4s.endpoint = &sam4s_ep;
	/* Construct the GPIO module. */
	struct _lf_module _gpio;
	_gpio.device = &sam4s;
	_gpio.identifier = _gpio_id;
	/* Execute the configure function. */
	uint32_t val = lf_invoke(&_gpio, _gpio_enable, fmr_args(fmr_int32(0xdeadbeef), fmr_int16(0xbabe)));
	printf("Got return value 0x%08x\n", val);
#endif

	return EXIT_SUCCESS;
}
