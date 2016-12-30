#ifndef __osmium_h__
#define __osmium_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* Include all of the module headers. */
#include <flipper/carbon/modules/adc.h>
#include <flipper/carbon/modules/button.h>
#include <flipper/carbon/modules/cpu.h>
#include <flipper/carbon/modules/dac.h>
#include <flipper/carbon/modules/error.h>
#include <flipper/carbon/modules/fld.h>
#include <flipper/fmr.h>
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

#ifdef __private_include__

#define OS_UART_BAUDRATE 115200

/* Initializes platform specific periphery and prepares system for kernel configuration. */
extern void system_init(void);
/* Performs a variety of system related tasks during idle. */
extern void system_task(void);
/* Deinitializes platform specific periphery and prepares system for shutdown. */
extern void system_deinit(void);
/* Resets the system. */
extern void system_reset(void);

#endif
#endif
