/* This is done in a separate compilation unit so that the atsam4s module index enumerations are correct. */

#include <flipper.h>
#include <flipper/atsam4s/modules.h>

int carbon_select_atsam4s(struct _lf_device *device) {
	// _adc.index = _adc_id;
	// _button.index = _button_id;
	// _dac.index = _dac_id;
	// _fld.index = _fld_id;
	// _gpio.index = _gpio_id;
	// _i2c.index = _i2c_id;
	// _led.index = _led_id;
	// _pwm.index = _pwm_id;
	// _rtc.index = _rtc_id;
	// _spi.index = _spi_id;
	// _swd.index = _swd_id;
	// _task.index = _task_id;
	// _temp.index = _temp_id;
	// _timer.index = _timer_id;
	// _uart0.index = _uart0_id;
	// _usart.index = _usart_id;
	// _usb.index = _usb_id;
	// _wdt.index = _wdt_id;
	return lf_success;
}
