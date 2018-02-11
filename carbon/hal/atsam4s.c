/* This is done in a separate compilation unit so that the atsam4s module index enumerations are correct. */

#define __private_include__
#include <flipper.h>
#include <flipper/atsam4s/modules.h>

int carbon_select_atsam4s(struct _lf_device *device) {
	LF_MODULE_SET_DEVICE_AND_ID(_adc, device, _adc_id);
	LF_MODULE_SET_DEVICE_AND_ID(_button, device, _button_id);
	LF_MODULE_SET_DEVICE_AND_ID(_dac, device, _dac_id);
	LF_MODULE_SET_DEVICE_AND_ID(_fld, device, _fld_id);
	LF_MODULE_SET_DEVICE_AND_ID(_gpio, device, _gpio_id);
	LF_MODULE_SET_DEVICE_AND_ID(_i2c, device, _i2c_id);
	LF_MODULE_SET_DEVICE_AND_ID(_led, device, _led_id);
	LF_MODULE_SET_DEVICE_AND_ID(_pwm, device, _pwm_id);
	LF_MODULE_SET_DEVICE_AND_ID(_rtc, device, _rtc_id);
	LF_MODULE_SET_DEVICE_AND_ID(_spi, device, _spi_id);
	LF_MODULE_SET_DEVICE_AND_ID(_swd, device, _swd_id);
	LF_MODULE_SET_DEVICE_AND_ID(_task, device, _task_id);
	LF_MODULE_SET_DEVICE_AND_ID(_temp, device, _temp_id);
	LF_MODULE_SET_DEVICE_AND_ID(_timer, device, _timer_id);
	LF_MODULE_SET_DEVICE_AND_ID(_uart0, device, _uart0_id);
	LF_MODULE_SET_DEVICE_AND_ID(_usart, device, _usart_id);
	LF_MODULE_SET_DEVICE_AND_ID(_usb, device, _usb_id);
	LF_MODULE_SET_DEVICE_AND_ID(_wdt, device, _wdt_id);
	return lf_success;
}
