/* This is done in a separate compilation unit so that the atmegau2 module index enumerations are correct. */

#define __private_include__
#include <flipper.h>
#include <flipper/atmegau2/modules.h>

int carbon_select_u2_gpio(struct _lf_device *device) {
	struct _carbon_context *context = device->_ctx;
	lf_assert(context, failure, E_NULL, "No context for selected carbon device.");
	struct _lf_device *u2 = context->_u2;
	LF_MODULE_SET_DEVICE_AND_ID(_gpio, u2, _gpio_id);
	return lf_success;
failure:
	return lf_error;
}

int carbon_select_atmegau2(struct _lf_device *device) {
	LF_MODULE_SET_DEVICE_AND_ID(_button, device, _button_id);
//	LF_MODULE_SET_DEVICE_AND_ID(_gpio, device, _gpio_id);
	LF_MODULE_SET_DEVICE_AND_ID(_led, device, _led_id);
	LF_MODULE_SET_DEVICE_AND_ID(_uart0, device, _uart0_id);
	LF_MODULE_SET_DEVICE_AND_ID(_wdt, device, _wdt_id);
	return lf_success;
}
