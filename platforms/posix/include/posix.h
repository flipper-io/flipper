#ifndef __lf_posix_h__
#define __lf_posix_h__

/* include the configuration header for this platform */
#include "lf_config.h"

/* Returns a list of all devices matching the flipper VID. */
struct _lf_ll *lf_libusb_get_devices(void);

#endif
