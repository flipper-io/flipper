/* Flipper Platform Support Header for the Atmel ATSAM4S */

/* NOTE: Only one target platform can be included at a time. */

/* Guard whether or not a target platform has already been defined. */
#ifndef __lf_platform_h__
#define __lf_platform_h__

#include <flipper/types.h>
/* Include the chip support headers. */
#include <flipper/atsam4s/asf/sam4s.h>

/* Define the modules that this platform uses. */
#define __use_adc__
#define __use_button__
#define __use_dac__
#define __use_fld__
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

/* Declare the LF_VAR and LF_FUNC types for this platform. */
#define LF_VAR __attribute__((section(".lf.vars")))
#define LF_FUNC __attribute__((section(".lf.funcs")))

/* Run at a base frequency of 96MHz. */
#define F_CPU 96000000
/* NOTE: The number of wait states is proportionate to the clock speed defined above. */
#define PLATFORM_WAIT_STATES 5

/* Clock generator settings for a 96MHz master clock. */
#define BOARD_OSCOUNT (CKGR_MOR_MOSCXTST(8))
#define BOARD_PLLBR (CKGR_PLLBR_MULB(60) | CKGR_PLLBR_PLLBCOUNT(1) | CKGR_PLLBR_DIVB(10))
#define BOARD_MCKR (PMC_MCKR_PRES_CLK_1 | PMC_MCKR_CSS_PLLB_CLK)

/* Define interrupt priorities, from highest (0) to lowest (15) priority. */
#define SYSTICK_PRIORITY 0
#define UART0_PRIORITY 1
#define PENDSV_PRIORITY 15

/* Communicate at 1 megabaud. */
#define PLATFORM_BAUDRATE 1000000

#define FMR_PIN PIO_PA0

#define FLASH_PCS 0
#define FLASH_PCS_PIN PIO_PA11A_NPCS0

#define USER_PCS 1
#define USER_PCS_PIN PIO_PA31A_NPCS1

#else
#error "Error: Multiple platforms targeted."
#endif
