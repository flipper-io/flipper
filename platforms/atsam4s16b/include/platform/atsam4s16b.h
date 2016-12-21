#ifndef __atsam4s16b_h__
#define __atsam4s16b_h__

#include <flipper/core.h>

#include "SAM4S16.h"
#include "exceptions.h"

/* A flipper device object which the target can use to keep track of state specific to itself. */
extern struct _lf_device self;

#define WEAK __attribute__((weak))

#define __use_fld__
#define __use_gpio__
#define __use_uart0__
#define __use_usart__

/* Clock generator settings for 48MHz master clock. */
#define BOARD_OSCOUNT (CKGR_MOR_MOSCXTST(8))
#define BOARD_PLLBR (CKGR_PLLBR_MULB(24) | CKGR_PLLBR_PLLBCOUNT(1) | CKGR_PLLBR_DIVB(10))
#define BOARD_MCKR (PMC_MCKR_PRES_CLK_1 | PMC_MCKR_CSS_PLLB_CLK)

#define CLOCK_TIMEOUT 5000

#define BOARD_MCK 48000000
#define PLATFORM_BAUDRATE 115200

#endif
