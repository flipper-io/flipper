#ifndef __atsam4s16b_h__
#define __atsam4s16b_h__

#include <flipper/core.h>

#include "atsam4s16b/SAM4S16.h"
#include "atsam4s16b/exceptions.h"
#include "atsam4s16b/cmsis/core_cm3.h"

/* A flipper device object which the target can use to keep track of state specific to itself. */
extern struct _lf_device self;

#define WEAK __attribute__((weak))

#define __use_adc__
#define __use_button__
// #define __use_cpu__
#define __use_dac__
#define __use_error__
#define __use_fld__
#define __use_fmr__
#define __use_fs__
#define __use_gpio__
#define __use_i2c__
#define __use_led__
#define __use_pwm__
#define __use_rtc__
#define __use_spi__
#define __use_swd__
#define __use_temp__
#define __use_timer__
#define __use_uart0__
#define __use_usart__
#define __use_usb__
#define __use_wdt__

/* Clock generator settings for 48MHz master clock. */
#define BOARD_OSCOUNT (CKGR_MOR_MOSCXTST(8))
#define BOARD_PLLBR (CKGR_PLLBR_MULB(24) | CKGR_PLLBR_PLLBCOUNT(1) | CKGR_PLLBR_DIVB(10))
#define BOARD_MCKR (PMC_MCKR_PRES_CLK_1 | PMC_MCKR_CSS_PLLB_CLK)

#define CLOCK_TIMEOUT 5000

#define F_CPU 48000000
#define PLATFORM_BAUDRATE 250000

#endif
