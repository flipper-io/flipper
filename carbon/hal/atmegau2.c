/* This is done in a separate compilation unit so that the atmegau2 module index enumerations are correct. */

#include <flipper.h>
#include <flipper/atmegau2/modules.h>

int carbon_select_u2_gpio(struct _lf_device *device) {
	struct _carbon_context *context = device->_ctx;
	lf_assert(context, failure, E_NULL, "No context for selected carbon device.");
	struct _lf_device *u2 = context->_u2;
	_gpio.index = _gpio_id;
	return lf_success;
failure:
	return lf_error;
}

int carbon_select_atmegau2(struct _lf_device *device) {
	_button.index = _button_id;;
//	_gpio.index = _gpio_id;;
	_led.index = _led_id;;
	_uart0.index = _uart0_id;;
	_wdt.index = _wdt_id;;
	return lf_success;
}
