#ifndef __lf_modules_h__
#define __lf_modules_h__

/* NOTE: This header file should only be included once by modules.c for each platform. */

/* Include the header files for all of the standard modules exposed by the toolbox. */
#include <flipper/adc.h>
#include <flipper/button.h>
#include <flipper/cpu.h>
#include <flipper/dac.h>
#include <flipper/error.h>
#include <flipper/fld.h>
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

/* Create an enumeraion defining all of the precomputed identifiers needed for the standard modules. */
enum {
	_adc_id,
	_button_id,
	_cpu_id,
	_dac_id,
	_error_id,
	_fld_id,
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
	/* The last element in a zero-indexed enumeration will be equal in value to its number of elements. */
	FMR_STD_MODULE_COUNT
};
/* NOTE: The identifiers in this enumeration must match the order of the module addresses stored in the 'fmr_modules' array. */

/* Define the standard modules based on platform specific usage declarations. */
static const void *const fmr_modules[FMR_STD_MODULE_COUNT] = {
	/* adc */
#ifdef __use_adc__
	&adc,
#else
	NULL,
#endif
	/* button */
#ifdef __use_button__
	&button,
#else
	NULL,
#endif
	/* cpu */
#ifdef __use_cpu__
	&cpu,
#else
	NULL,
#endif
	/* dac */
#ifdef __use_dac__
	&dac,
#else
	NULL,
#endif
	/* error */
#ifdef __use_error__
	&error,
#else
	NULL,
#endif
	/* fld */
#ifdef __use_fld__
	&fld,
#else
	NULL,
#endif
	/* fmr */
#ifdef __use_fmr__
	&fmr,
#else
	NULL,
#endif
	/* fs */
#ifdef __use_fs__
	&fs,
#else
	NULL,
#endif
	/* gpio */
#ifdef __use_gpio__
	&gpio,
#else
	NULL,
#endif
	/* i2c */
#ifdef __use_i2c__
	&i2c,
#else
	NULL,
#endif
	/* led */
#ifdef __use_led__
	&led,
#else
	NULL,
#endif
	/* pwm */
#ifdef __use_pwm__
	&pwm,
#else
	NULL,
#endif
	/* rtc */
#ifdef __use_rtc__
	&rtc,
#else
	NULL,
#endif
	/* spi */
#ifdef __use_spi__
	&spi,
#else
	NULL,
#endif
	/* swd */
#ifdef __use_swd__
	&swd,
#else
	NULL,
#endif
	/* temp */
#ifdef __use_temp__
	&temp,
#else
	NULL,
#endif
	/* timer */
#ifdef __use_timer__
	&timer,
#else
	NULL,
#endif
	/* uart0 */
#ifdef __use_uart0__
	&uart0,
#else
	NULL,
#endif
	/* usart */
#ifdef __use_usart__
	&usart,
#else
	NULL,
#endif
	/* usb */
#ifdef __use_usb__
	&usb,
#else
	NULL,
#endif
	/* wdt */
#ifdef __use_wdt__
	&wdt,
#else
	NULL,
#endif
};

#endif
