#ifndef __lf_posix_h__
#define __lf_posix_h__

/* Returns a list of all devices matching the flipper VID. */
struct _lf_ll *lf_libusb_get_devices(void);

#endif
