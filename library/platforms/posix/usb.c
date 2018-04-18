/* libusb.c - USB endpoint wrapper using libusb. */

#include <flipper.h>
#include <libusb.h>

struct _lf_libusb_context {
	struct libusb_device_handle *handle;
	struct libusb_context *context;
};

int lf_libusb_read(struct _lf_device *device, void *dst, uint32_t length) {
	lf_assert(device, failure, E_NULL, "No device provided to '%s'", __PRETTY_FUNCTION__);

	struct _lf_libusb_context *context = (struct _lf_libusb_context *)device->_ep_ctx;
	lf_assert(context, failure, E_NULL, "No context provided in '%s'.", __PRETTY_FUNCTION__);

	int transferred;
	int e;
	int total = lf_ceiling(length, BULK_IN_SIZE);
	for (int i = 0; i < total; i ++) {
		uint8_t data[BULK_IN_SIZE];
		e = libusb_bulk_transfer(context->handle, BULK_IN_ENDPOINT, data, BULK_IN_SIZE, &transferred, LF_USB_TIMEOUT_MS);
		if (e != 0) {
			if (e == LIBUSB_ERROR_TIMEOUT) {
				lf_error_raise(E_TIMEOUT, error_message("The transfer to the device timed out."));
			} else {
				lf_error_raise(E_COMMUNICATION, error_message("Error during libusb transfer."));
			}
			return lf_error;
		}
		if (length >= BULK_IN_SIZE) {
			memcpy(dst, data, BULK_IN_SIZE);
		} else {
			memcpy(dst, data, length);
		}
		dst += transferred;
		length -= transferred;
	}
	return lf_success;

failure:
	return lf_error;
}

int lf_libusb_write(struct _lf_device *device, void *src, uint32_t length) {
	lf_assert(device, failure, E_NULL, "No device provided to '%s'", __PRETTY_FUNCTION__);

	struct _lf_libusb_context *context = (struct _lf_libusb_context *)device->_ep_ctx;
	lf_assert(context, failure, E_NULL, "No context provided in '%s'.", __PRETTY_FUNCTION__);

	int transferred;
	int e;
	int total = lf_ceiling(length, BULK_IN_SIZE);
	for (int i = 0; i < total; i ++) {
		uint8_t data[BULK_OUT_SIZE];
		if (length >= BULK_OUT_SIZE) {
			memcpy(data, src, BULK_OUT_SIZE);
		} else {
			memcpy(data, src, length);
		}
		e = libusb_bulk_transfer(context->handle, BULK_OUT_ENDPOINT, data, BULK_OUT_SIZE, &transferred, LF_USB_TIMEOUT_MS);
		if (e != 0) {
			if (e == LIBUSB_ERROR_TIMEOUT) {
				lf_error_raise(E_TIMEOUT, error_message("The transfer to the device timed out."));
			} else {
				lf_error_raise(E_COMMUNICATION, error_message("Error during libusb transfer."));
			}
			return lf_error;
		}
		src += transferred;
		length -= transferred;
	}
	return lf_success;

failure:
	return lf_error;
}

int lf_libusb_release(struct _lf_device *device) {
	lf_assert(device, failure, E_NULL, "No device specified for '%s'.", __PRETTY_FUNCTION__);

	struct _lf_libusb_context *context = (struct _lf_libusb_context *)device->_ep_ctx;
	lf_assert(context, failure, E_NULL, "No context specified for '%s'.", __PRETTY_FUNCTION__);
	libusb_close(context->handle);
	libusb_exit(context->context);
	return lf_success;

failure:
	return lf_error;
}

struct _lf_ll *lf_libusb_devices_for_vid_pid(uint16_t vid, uint16_t pid) {
	struct libusb_context *context = NULL;
	struct libusb_device **libusb_devices = NULL;
	struct libusb_device *libusb_device = NULL;
	struct libusb_device_descriptor descriptor;
	struct _lf_ll *devices = NULL;

	int e = libusb_init(&context);
	lf_assert(e == 0, failure, E_LIBUSB, "Failed to initialize libusb. Reboot and try again.");

	size_t device_count = libusb_get_device_list(context, &libusb_devices);
	for (size_t i = 0; i < device_count; i ++) {
		libusb_device = libusb_devices[i];

		e = libusb_get_device_descriptor(libusb_device, &descriptor);
		lf_assert(e == 0, failure, E_LIBUSB, "Failed to obtain descriptor for device.");

		if (descriptor.idVendor == vid && descriptor.idProduct == pid) {
			struct _lf_device *device = lf_device_create(lf_libusb_read, lf_libusb_write, lf_libusb_release);
			lf_assert(device, failure, E_ENDPOINT, "Failed to create device in '%s'.", __PRETTY_FUNCTION__);
			device->_ep_ctx = calloc(1, sizeof(struct _lf_network_context));
			struct _lf_libusb_context *context = (struct _lf_libusb_context *)device->_ep_ctx;
			lf_assert(context, failure, E_NULL, "Failed to allocate memory for context in '%s'.", __PRETTY_FUNCTION__);

			e = libusb_open(libusb_device, &(context->handle));
			lf_assert(e == 0, failure, E_NO_DEVICE, "Could not find any devices connected via USB. Ensure that a device is connected.");

			e = libusb_claim_interface(context->handle, FMR_INTERFACE);
			lf_assert(e == 0, failure, E_LIBUSB, "Failed to claim interface on attached device. Please quit any other programs using your device.");

			e = lf_ll_append(&devices, device, lf_device_release);
			lf_assert(e == 0, failure, E_NULL, "Failed to attach device.");
		}
	}

	return devices;

failure:
	lf_ll_release(&devices);
	libusb_free_device_list(libusb_devices, 1);
	libusb_exit(context);
	return NULL;
}
