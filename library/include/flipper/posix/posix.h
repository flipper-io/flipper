#ifndef __posix_h__
#define __posix_h__

/* Top-level POSIX platform support header. */

#include <flipper/posix/network.h>
#include <flipper/posix/libusb.h>

/* Terminal colors. */
#define KNRM  "\x1B[0m"
#define KGRN  "\x1B[32m"
#define KRED  "\x1B[31m"
#define KBLU  "\x1B[34m"
#define KYEL  "\x1B[33m"

#define __use_adc__
#define __use_button__
#define __use_cpu__
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
#define __use_task__
#define __use_temp__
#define __use_timer__
#define __use_uart0__
#define __use_usart__
#define __use_usb__
#define __use_wdt__

#endif
