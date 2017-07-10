#ifndef __lf_libusb_h__
#define __lf_libusb_h__

#include <flipper/types.h>
#include <flipper/ll.h>

/* Attaches to all devices with a given VID and PID. */
struct _lf_ll *lf_libusb_endpoints_for_vid_pid(uint16_t vid, uint16_t pid);

/* ~ Declare the prototypes for all functions exposed by this driver. ~ */
int lf_libusb_configure();
uint8_t lf_libusb_ready(struct _lf_endpoint *endpoint);
void lf_libusb_put(struct _lf_endpoint *endpoint, uint8_t byte);
uint8_t lf_libusb_get(struct _lf_endpoint *endpoint);
int lf_libusb_push(struct _lf_endpoint *endpoint, void *source, lf_size_t length);
int lf_libusb_pull(struct _lf_endpoint *endpoint, void *destination, lf_size_t length);
int lf_libusb_destroy(struct _lf_endpoint *endpoint);

#endif
