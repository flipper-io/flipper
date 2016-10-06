#ifndef _LIB_SAM4S_
#define _LIB_SAM4S_

#define RAMFUNC __attribute__ ((section (".ramfunc")))
#define WEAK __attribute__ ((weak))

#include "SAM4S16.h"

#include "include/exceptions.h"
#include "include/acc.h"
#include "include/adc.h"
#include "include/async.h"
#include "include/crccu.h"
#include "include/dacc.h"
#include "include/efc.h"
#include "include/flashd.h"
#include "include/hsmci.h"
#include "include/pio.h"
#include "include/pio_it.h"
#include "include/pio_capture.h"
#include "include/pmc.h"
#include "include/pwmc.h"
#include "include/rtc.h"
#include "include/rtt.h"
#include "include/spi.h"
#include "include/spi_pdc.h"
#include "include/ssc.h"
#include "include/tc.h"
#include "include/twi.h"
#include "include/twid.h"
#include "include/usart.h"
#include "include/trace.h"
#include "include/wdt.h"
#include "include/USBD_Config.h"

#endif

