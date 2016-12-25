#ifndef __lf_usb_h__
#define __lf_usb_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* ~ Declare the virtual interface for this driver. ~ */
extern struct _lf_endpoint lf_usb_ep;

#ifdef __private_include__

/* ~ Declare the prototypes for all functions exposed by this driver. ~ */
int lf_usb_configure(struct _lf_endpoint *endpoint);
uint8_t lf_usb_ready(void);
void lf_usb_put(uint8_t byte);
uint8_t lf_usb_get(void);
int lf_usb_push(void *source, lf_size_t length);
int lf_usb_pull(void *destination, lf_size_t length);
int lf_usb_destroy(struct _lf_endpoint *endpoint);

#endif
#endif
