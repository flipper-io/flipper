#ifndef __lf_usb_h__
#define __lf_usb_h__

struct _lf_libusb_context {
	struct libusb_device_handle *handle;
	struct libusb_context *context;
    uint8_t in_sz, out_sz;
    uint8_t in, out;
};

/* Returns a list of all devices matching the flipper VID. */
struct _lf_ll *lf_libusb_get_devices(void);

#endif
