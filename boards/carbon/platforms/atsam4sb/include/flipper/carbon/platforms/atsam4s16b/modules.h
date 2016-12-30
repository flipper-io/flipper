#ifndef __lf_modules_h__
#define __lf_modules_h__

/* NOTE: This header file should only be included once by modules.c for each platform. */

/* Include the header files for all of the standard modules exposed by the toolbox. */
#include <flipper/carbon/adc.h>
#include <flipper/carbon/button.h>
#include <flipper/carbon/cpu.h>
#include <flipper/carbon/dac.h>
#include <flipper/carbon/error.h>
#include <flipper/carbon/fld.h>
#include <flipper/fmr.h>
#include <flipper/carbon/fs.h>
#include <flipper/carbon/gpio.h>
#include <flipper/carbon/i2c.h>
#include <flipper/carbon/led.h>
#include <flipper/carbon/pwm.h>
#include <flipper/carbon/rtc.h>
#include <flipper/carbon/spi.h>
#include <flipper/carbon/swd.h>
#include <flipper/carbon/temp.h>
#include <flipper/carbon/timer.h>
#include <flipper/carbon/uart0.h>
#include <flipper/carbon/usart.h>
#include <flipper/carbon/usb.h>
#include <flipper/carbon/wdt.h>

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
	#define __use_fmr__
	_fmr_id,
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

#endif
