/* Guard whether or not a target platform has already been defined. */
#ifndef __lf_platform_h__
#define __lf_platform_h__

#include <unistd.h>
#include <flipper/posix/network.h>
#include <flipper/posix/usb.h>

/* Define the modules that this platform uses. */
#define __use_adc__
#define __use_button__
#define __use_cpu__
#define __use_dac__
#define __use_error__
#define __use_fld__
#define __use_fmr__
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

#else
#error "Error: Multiple platforms targeted."
#endif
