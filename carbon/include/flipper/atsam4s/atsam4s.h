#ifndef __atsam4s16b_h__
#define __atsam4s16b_h__

#include <flipper/libflipper.h>

#include <flipper/atsam4s/SAM4S16.h>

#ifdef __ATSAM4SB__
#include <exceptions.h>
#endif

#define __use_adc__
#define __use_button__
#define __use_dac__
#define __use_fld__
#define __use_fs__
#define __use_gpio__
#define __use_i2c__
#define __use_led__
#define __use_pwm__
#define __use_rtc__
#define __use_spi__
#define __use_swd__
#define __use_task__
#define __use_temp__
#define __use_timer__
#define __use_uart0__
#define __use_usart__
#define __use_usb__
#define __use_wdt__

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
