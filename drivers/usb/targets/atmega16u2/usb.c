#define __private_include__
#include <flipper/usb.h>
#include <flipper/platform/hid.h>
#include <flipper/platform/platform.h>

void usb_configure() {

	configure_usb();

}

void usb_enable(void) {

}

void usb_disable(void) {

}

uint8_t usb_ready(void) {

	return 0;

}

void usb_put(uint8_t byte) {

}

uint8_t usb_get(void) {

	return 0;

}

void usb_push(void *source, size_t length) {

	usb_send_packet(source);

}

void usb_pull(void *destination, size_t length) {

	uint8_t size = 0;

	while (!size) size = usb_receive_packet((uint8_t *)(destination));

}
