#ifndef __atsam4s16b_h__
#define __atsam4s16b_h__

#include "chip.h"

/* A flipper device object which the target can use to keep track of state specific to itself. */
extern struct _lf_device self;

#define __use_fld__
#define __use_gpio__
#define __use_uart0__
#define __use_usart__

#define FMR_MODULES_ATTRIBUTES

#define BOARD_MCK 48000000
#define PLATFORM_BAUDRATE 115200

#endif
