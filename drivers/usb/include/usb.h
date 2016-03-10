#ifndef __usb_h__
#define __usb_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* ~ Declare the virtual driver object. ~ */
extern const struct _bus usb;

#ifdef __private_include__

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _usb_configure, _usb_enable, _usb_disable, _usb_ready, _usb_put, _usb_get, _usb_push, _usb_pull };

/* ~ Declare all function prototypes for this driver. ~ */
extern void usb_configure(void);
extern void usb_enable(void);
extern void usb_disable(void);
extern bool usb_ready(void);
extern void usb_put(uint8_t byte);
extern uint8_t usb_get(void);
extern void usb_push(void *source, uint32_t length);
extern void usb_pull(void *destination, uint32_t length);

#endif
#endif
