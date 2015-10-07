#define __private_include__

#include <usb/usb.h>

const struct _bus usb = {
	
	usb_configure,
	
	usb_enable,
	
	usb_disable,
	
	usb_ready,
	
	usb_put,
	
	usb_get,
	
	usb_push,
	
	usb_pull,
	
	true
	
};