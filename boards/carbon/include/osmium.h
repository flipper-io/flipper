#ifndef __osmium_h__
#define __osmium_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* Include all of the module headers. */
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
