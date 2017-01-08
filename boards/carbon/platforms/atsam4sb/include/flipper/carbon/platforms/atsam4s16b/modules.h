#ifndef __lf_modules_h__
#define __lf_modules_h__

#ifdef __private_include__

/* Create an enumeraion defining all of the precomputed identifiers needed for the standard modules. */
enum {
	#define __use_adc__
	_adc_id,
	#define __use_button__
	_button_id,
	#define __use_dac__
	_dac_id,
	#define __use_fld__
	_fld_id,
	#define __use_fs__
	_fs_id,
	#define __use_gpio__
	_gpio_id,
	#define __use_i2c__
	_i2c_id,
	#define __use_led__
	_led_id,
	#define __use_pwm__
	_pwm_id,
	#define __use_rtc__
	_rtc_id,
	#define __use_spi__
	_spi_id,
	#define __use_swd__
	_swd_id,
	#define __use_task__
	_task_id,
	#define __use_temp__
	_temp_id,
	#define __use_timer__
	_timer_id,
	#define __use_usart__
	_usart_id,
	#define __use_usb__
	_usb_id,
	#define __use_wdt__
	_wdt_id,
};
/* NOTE: The identifiers in this enumeration must match the order of the module addresses stored in the 'fmr_modules' array. */

/* For use internal to the platform. */
#define __use_uart0__

#endif
#endif
