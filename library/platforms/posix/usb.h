#ifndef __lf_usb_h__
#define __lf_usb_h__

/* Attaches to all devices with a given VID and PID. */
struct _lf_ll *lf_libusb_devices_for_vid_pid(uint16_t vid, uint16_t pid);

#endif
