#ifndef __platform_h__
#define __platform_h__

#include "chip.h"

#include <flipper/uart0.h>

/* A flipper device object which the target can use to keep track of state specific to itself. */
extern struct _lf_device self;

#define BOARD_MCK 48000000
#define PLATFORM_BAUDRATE 115200

#endif
