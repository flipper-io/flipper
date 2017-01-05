#ifndef __lf_modules_h__
#define __lf_modules_h__

/* Include the header files for all of the standard modules exposed by the toolbox. */
#include <flipper/carbon/modules/adc.h>
#include <flipper/carbon/modules/button.h>
#include <flipper/carbon/modules/cpu.h>
#include <flipper/carbon/modules/dac.h>
#include <flipper/carbon/modules/fld.h>
#include <flipper/carbon/modules/fs.h>
#include <flipper/carbon/modules/gpio.h>
#include <flipper/carbon/modules/i2c.h>
#include <flipper/carbon/modules/led.h>
#include <flipper/carbon/modules/pwm.h>
#include <flipper/carbon/modules/rtc.h>
#include <flipper/carbon/modules/spi.h>
#include <flipper/carbon/modules/swd.h>
#include <flipper/carbon/modules/temp.h>
#include <flipper/carbon/modules/timer.h>
#include <flipper/carbon/modules/uart0.h>
#include <flipper/carbon/modules/usart.h>
#include <flipper/carbon/modules/usb.h>
#include <flipper/carbon/modules/wdt.h>

/* NOTE: This include should only be here until FMR is removed as a module. */
#include <flipper/fmr.h>

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
