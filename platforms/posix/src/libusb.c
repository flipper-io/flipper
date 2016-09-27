/* libusb.c - USB endpoint wrapper using libusb. */

#define __private_include__
#include <flipper/flipper.h>
#include <platform/posix/libusb.h>
#include <libusb.h>
#include <flipper/error.h>

struct _lf_endpoint lf_usb_ep = {
	lf_usb_configure,
	lf_usb_ready,
	lf_usb_put,
	lf_usb_get,
	lf_usb_push,
	lf_usb_pull,
	lf_usb_destroy
};

struct _lf_usb_record {
	struct libusb_device_handle *handle;
	struct libusb_context *context;
};

int lf_usb_configure(struct _lf_endpoint *endpoint) {
	if (!endpoint) {
		error_raise(E_NULL, error_message("No endpoint record provided for libusb initialization."));
		return lf_error;
	}
	/* Allocate memory for the USB record if it has not yet been allocated. */
	if (!(endpoint -> record)) {
		endpoint -> record = malloc(sizeof(struct _lf_usb_record));
	}
	struct _lf_usb_record *record = endpoint -> record;
	/* Initialize the libusb context associated with this endpoint. */
	int _e = libusb_init(&(record -> context));
	if (_e < 0) {
		error_raise(E_LIBUSB, error_message("Failed to initialize libusb."));
		return lf_error;
	}
	/* Attach a physical device to this endpoint. */
	record -> handle = libusb_open_device_with_vid_pid(record -> context, USB_VENDOR_ID, USB_PRODUCT_ID);
	if (!(record -> handle)) {
		return lf_error;
	}
	/* Claim the interface used to send and receive message runtime packets. */
	_e = libusb_claim_interface(record -> handle, 0);
	if (_e < 0) {
		error_raise(E_LIBUSB, error_message("Failed to claim interface 0 on attached device."));
		return lf_error;
	}
	libusb_set_debug(record -> context, 3);
	/* Reset the device's USB controller. */
	libusb_reset_device(record -> handle);
	return lf_success;
}

uint8_t lf_usb_ready(void) {

	return 0;
}

void lf_usb_put(uint8_t byte) {

}

uint8_t lf_usb_get(void) {

	return 0;
}

int lf_usb_transfer(void *data, lf_size_t length, uint8_t endpoint) {
	struct _lf_usb_record *record = lf_device() -> endpoint -> record;
	if (!record) {
		error_raise(E_ENDPOINT, error_message("No libusb record associated with the selected USB endpoint. Did you attach?"));
		return lf_error;
	}
	int _length, _e;
	if (endpoint == INTERRUPT_IN_ENDPOINT || endpoint == INTERRUPT_OUT_ENDPOINT) {
		if (endpoint == INTERRUPT_IN_ENDPOINT) {
			length = INTERRUPT_IN_SIZE;
		}
		_e = libusb_interrupt_transfer(record -> handle, endpoint, data, length, &_length, 0);
	} else if (endpoint == BULK_IN_ENDPOINT || endpoint == BULK_OUT_ENDPOINT) {
		_e = libusb_bulk_transfer(record -> handle, endpoint, data, length, &_length, 0);
	} else {
		error_raise(E_ENDPOINT, error_message("An invalid endpoint (0x%02x) was provided for USB transfer.", endpoint));
		return lf_error;
	}
	if (_e < 0 || (_length != length)) {
		error_raise(E_COMMUNICATION, error_message("Failed to communicate with USB device. Connection interrupted."));
		return lf_error;
	}
	return lf_success;
}

int lf_usb_push(void *source, lf_size_t length) {
	return lf_usb_transfer(source, length, INTERRUPT_OUT_ENDPOINT);
}

int lf_usb_pull(void *destination, lf_size_t length) {
	return lf_usb_transfer(destination, length, INTERRUPT_IN_ENDPOINT);
}

int lf_usb_destroy(struct _lf_endpoint *endpoint) {
	struct _lf_usb_record *record = lf_device() -> endpoint -> record;
	if (!record) {
		error_raise(E_ENDPOINT, error_message("No libusb record associated with the selected USB endpoint. Did you attach?"));
		return lf_error;
	}
	/* Close the device handle. */
	libusb_close(record -> handle);
	/* Destroy the libusb context. */
	libusb_exit(record -> context);
	return lf_success;
}
