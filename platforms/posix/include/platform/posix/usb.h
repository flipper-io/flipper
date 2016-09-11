#ifndef __usb_h__
#define __usb_h__

#include <flipper/core.h>

extern const struct _lf_endpoint usb;

#ifdef __private_include__

#define VENDOR  0x16C0
#define PRODUCT 0x0480

#define INTERRUPT_ENDPOINT	0x01
#define BULK_ENDPOINT		0x03

int usb_configure(struct _lf_endpoint *endpoint);
uint8_t usb_ready(void);
void usb_put(uint8_t byte);
uint8_t usb_get(void);
int usb_push(void *source, lf_size_t length);
int usb_pull(void *destination, lf_size_t length);
int usb_destroy(struct _lf_endpoint *endpoint);

#endif
#endif
