#ifndef __lf_modules_h__
#define __lf_modules_h__

/* Include the header files for all of the standard modules exposed by the toolbox. */
#include <flipper/adc.h>
#include <flipper/button.h>
#include <flipper/cpu.h>
#include <flipper/dac.h>
#include <flipper/error.h>
#include <flipper/fmr.h>
#include <flipper/fs.h>
#include <flipper/gpio.h>
#include <flipper/i2c.h>
#include <flipper/led.h>
#include <flipper/pwm.h>
#include <flipper/rtc.h>
#include <flipper/spi.h>
#include <flipper/swd.h>
#include <flipper/temp.h>
#include <flipper/timer.h>
#include <flipper/uart0.h>
#include <flipper/usart.h>
#include <flipper/usb.h>
#include <flipper/wdt.h>

#define _forward_id (const void *)(-1)

/* Create an enumeraion defining all precomputed identifiers for the provided standard modules. */
enum {
	_adc_id,
	_button_id,
	_cpu_id,
	_dac_id,
	_error_id,
	_fmr_id,
	_fs_id,
	_gpio_id,
	_i2c_id,
	_led_id,
	_pwm_id,
	_rtc_id,
	_spi_id,
	_swd_id,
	_temp_id,
	_timer_id,
	_uart0_id,
	_usart_id,
	_usb_id,
	_wdt_id,
	_std_module_id_max,
};

/* Describe a reference to the platform specific module-array. */
extern const void *const fmr_modules[];
/* NOTE: The platform specific function 'lf_std_function(lf_id_t module)' should be used to access objects within this array. */

#endif
