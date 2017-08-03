#ifndef __atsam4s16b_h__
#define __atsam4s16b_h__

#include <flipper/libflipper.h>

#include <flipper/atsam4s/SAM4S16.h>

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
	#define __use_uart0__
	_uart0_id,
	#define __use_usart__
	_usart_id,
	#define __use_usb__
	_usb_id,
	#define __use_wdt__
	_wdt_id,
};
/* NOTE: The identifiers in this enumeration must match the order of the module addresses stored in the 'fmr_modules' array. */

#include <exceptions.h>

/* Clock the CM4 CPU at 96 MHz. */
#define F_CPU 96000000
/* NOTE: The number of wait states is proportionate to the clock speed defined above. */
#define PLATFORM_WAIT_STATES 5

/* Clock generator settings for a 96MHz master clock. */
#define BOARD_OSCOUNT (CKGR_MOR_MOSCXTST(8))
#define BOARD_PLLBR (CKGR_PLLBR_MULB(48) | CKGR_PLLBR_PLLBCOUNT(1) | CKGR_PLLBR_DIVB(10))
#define BOARD_MCKR (PMC_MCKR_PRES_CLK_1 | PMC_MCKR_CSS_PLLB_CLK)

#define CLOCK_TIMEOUT 5000

/* Define interrupt priorities, from highest (0) to lowest (15) priority. */
#define SYSTICK_PRIORITY 0
#define UART0_PRIORITY 1
#define PENDSV_PRIORITY 15

/* 2 megabaud. */
#define PLATFORM_BAUDRATE 2000000

#endif
