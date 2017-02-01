#ifndef __atsam4s16b_h__
#define __atsam4s16b_h__

#include <flipper/libflipper.h>

#include "atsam4s16b/SAM4S16.h"
#include "atsam4s16b/cmsis/core_cm3.h"

/* Include the module declarations for this platform. */
#include "atsam4s16b/modules.h"

#ifdef __private_include__

#include "atsam4s16b/exceptions.h"

/* A flipper device object which the target can use to keep track of state specific to itself. */
extern struct _lf_device self;

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
#endif
