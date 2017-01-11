/* libusb.c - USB endpoint wrapper using libusb. */

#define __private_include__
#include <flipper/libflipper.h>
#include <flipper/platforms/posix/libusb.h>
#include <libusb.h>

struct _lf_endpoint lf_libusb_ep = {
	lf_libusb_configure,
	lf_libusb_ready,
	lf_libusb_put,
	lf_libusb_get,
	lf_libusb_push,
	lf_libusb_pull,
	lf_libusb_destroy
};

struct _lf_libusb_record {
	struct libusb_device_handle *handle;
	struct libusb_context *context;
};

int lf_libusb_configure(struct _lf_endpoint *this, struct _lf_device *device) {
	if (!this) {
		lf_error_raise(E_NULL, error_message("No endpoint record provided for libusb configuration. Reattach your device and try again."));
		return lf_error;
	}
	/* Allocate memory for the USB record if it has not yet been allocated. */
	if (!(this -> record)) {
		this -> record = calloc(1, sizeof(struct _lf_libusb_record));
		if (!(this -> record)) {
			lf_error_raise(E_MALLOC, error_message("Failed to allocate the memory needed to create a libusb record."));
			goto failure;
		}
	}
	struct _lf_libusb_record *record = this -> record;
	/* Initialize the libusb context associated with this endpoint. */
	int _e = libusb_init(&(record -> context));
	if (_e < 0) {
		lf_error_raise(E_LIBUSB, error_message("Failed to initialize libusb. Reboot and try again."));
		goto failure;
	}
	/* Attach a physical device to this endpoint. */
	record -> handle = libusb_open_device_with_vid_pid(record -> context, LF_USB_VENDOR_ID, LF_USB_PRODUCT_ID);
	if (!(record -> handle)) {
		lf_error_raise(E_NO_DEVICE, error_message("Could not find any devices connected via USB. Ensure that a device is connected."));
		goto failure;
	}
	/* Claim the interface used to send and receive message runtime packets. */
	_e = libusb_claim_interface(record -> handle, 0);
	if (_e < 0) {
		lf_error_raise(E_LIBUSB, error_message("Failed to claim interface on attached device. Please quit any other programs using your device."));
		goto failure;
	}

	/* Broadcast a packet to the device over its endpoint to verify the identifier. */
	_e = lf_load_configuration(device);
	if (_e < lf_success) {
		lf_error_raise(E_CONFIGURATION, error_message("Failed to obtain configuration for device '%s'.", device -> configuration.name));
		goto failure;
	}

	libusb_set_debug(record -> context, 3);
	/* Reset the device's USB controller. */
	libusb_reset_device(record -> handle);

	return lf_success;

failure:

	lf_libusb_destroy(this);

	return lf_error;
}

uint8_t lf_libusb_ready(struct _lf_endpoint *this) {
	return 0;
}

void lf_libusb_put(struct _lf_endpoint *this, uint8_t byte) {
	return;
}

uint8_t lf_libusb_get(struct _lf_endpoint *this) {
	return 0;
}

int lf_libusb_push(struct _lf_endpoint *this, void *source, lf_size_t length) {
	struct _lf_libusb_record *record = this -> record;
	int _length;
	int _e;
#ifndef __ALL_BULK__
	if (length <= INTERRUPT_OUT_SIZE) {
		_e = libusb_interrupt_transfer(record -> handle, INTERRUPT_OUT_ENDPOINT, source, length, &_length, LF_USB_TIMEOUT_MS);
	} else {
#endif
		_e = libusb_bulk_transfer(record -> handle, BULK_OUT_ENDPOINT, source, length, &_length, LF_USB_TIMEOUT_MS);
#ifndef __ALL_BULK__
	}
#endif
	if (_e < 0) {
		if (_e == LIBUSB_ERROR_TIMEOUT) {
			lf_error_raise(E_TIMEOUT, error_message("The transfer to the device timed out."));
		} else {
			lf_error_raise(E_COMMUNICATION, error_message("Error during libusb transfer."));
		}
		return lf_error;
	} else if (_length != length) {
		lf_error_raise(E_COMMUNICATION, error_message("Failed to transmit complete USB packet."));
		return lf_error;
	}
	return lf_success;
}

int lf_libusb_pull(struct _lf_endpoint *this, void *destination, lf_size_t length) {
	struct _lf_libusb_record *record = this -> record;
	int _length;
	int _e;
#ifndef __ALL_BULK__
	if (length <= INTERRUPT_IN_SIZE) {
		_e = libusb_interrupt_transfer(record -> handle, INTERRUPT_IN_ENDPOINT, destination, length, &_length, LF_USB_TIMEOUT_MS);
	} else {
#endif
		_e = libusb_bulk_transfer(record -> handle, BULK_IN_ENDPOINT, destination, length, &_length, LF_USB_TIMEOUT_MS);
#ifndef __ALL_BULK__
	}
#endif
	if (_e < 0) {
		if (_e == LIBUSB_ERROR_TIMEOUT) {
			lf_error_raise(E_TIMEOUT, error_message("The transfer to the device timed out."));
		} else {
			lf_error_raise(E_COMMUNICATION, error_message("Error during libusb transfer."));
		}
		return lf_error;
	} else if (_length != length) {
		lf_error_raise(E_COMMUNICATION, error_message("Failed to receive complete USB packet. (%d bytes / %d bytes)", _length, length));
		return lf_error;
	}
	return lf_success;
}

int lf_libusb_destroy(struct _lf_endpoint *this) {
	struct _lf_libusb_record *record = this -> record;
	if (record) {
		if (record -> handle) {
			/* Close the device handle. */
			libusb_close(record -> handle);
		}
		if (record -> context) {
			/* Destroy the libusb context. */
			libusb_exit(record -> context);
		}
		/* Free the record. */
		free(record);
	}
	return lf_success;
}
