#ifndef __usb_h__

#define __usb_h__

#include <types.h>

#include <fmr/bus.h>

extern const struct _bus usb;

#ifdef __private_include__

#define CARBON_VENDOR_ID	0x16C0

#define CARBON_PRODUCT_ID	0x0480

#define CARBON_USAGE_PAGE	0xFFAB

#define CARBON_USAGE		0x0200

#define USB_PACKET_LENGTH	32

enum { _usb_configure, _usb_enable, _usb_disable, _usb_ready, _usb_put, _usb_get, _usb_push, _usb_pull };

extern void usb_configure(uint16_t configuration);

extern void usb_enable(void);

extern void usb_disable(void);

extern bool usb_ready(void);

extern void usb_put(uint8_t byte);

extern uint8_t usb_get(void);

extern void usb_push(void *source, uint32_t length);

extern void usb_pull(void *destination, uint32_t length);


extern int hid_enumerate(int max, int vid, int pid, int usage_page, int usage);

extern int8_t hid_receive_packet(uint8_t *buffer);

extern int8_t hid_transmit_packet(uint8_t *buffer);

extern void hid_detach(int num);

#endif

#endif