#define __private_include__
#include <usb/usb.h>
#include <platform.h>
#include <hid.h>

void usb_configure() {

	configure_usb();

}

void usb_enable(void) {

}

void usb_disable(void) {

}

bool usb_ready(void) {

	return 0;

}

void usb_put(uint8_t byte) {

}

uint8_t usb_get(void) {

	return 0;

}

void usb_push(void *source, uint32_t length) {

	usb_send_packet(source);

}

void usb_pull(void *destination, uint32_t length) {

	uint8_t size = 0;

	while (!size) size = usb_receive_packet((uint8_t *)(destination));

}
