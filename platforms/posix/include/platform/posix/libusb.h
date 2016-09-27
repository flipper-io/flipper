#ifndef __lf_usb_h__
#define __lf_usb_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* ~ Declare the virtual interface for this driver. ~ */
extern struct _lf_endpoint libusb;

#ifdef __private_include__

/* ~ Declare the prototypes for all functions exposed by this driver. ~ */
int libusb_configure(struct _lf_endpoint *endpoint);
uint8_t libusb_ready(void);
void libusb_put(uint8_t byte);
uint8_t libusb_get(void);
int libusb_push(void *source, lf_size_t length);
int libusb_pull(void *destination, lf_size_t length);
int libusb_destroy(struct _lf_endpoint *endpoint);

#endif
#endif
