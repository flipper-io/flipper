#ifndef __lf_libusb_h__
#define __lf_libusb_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/libflipper.h>

/* ~ Declare the virtual interface for this driver. ~ */
extern struct _lf_endpoint lf_libusb_ep;

#ifdef __private_include__

/* ~ Declare the prototypes for all functions exposed by this driver. ~ */
int lf_libusb_configure(struct _lf_endpoint *this);
uint8_t lf_libusb_ready(struct _lf_endpoint *this);
void lf_libusb_put(struct _lf_endpoint *this, uint8_t byte);
uint8_t lf_libusb_get(struct _lf_endpoint *this);
int lf_libusb_push(struct _lf_endpoint *this, void *source, lf_size_t length);
int lf_libusb_pull(struct _lf_endpoint *this, void *destination, lf_size_t length);
int lf_libusb_destroy(struct _lf_endpoint *this);

#endif
#endif
