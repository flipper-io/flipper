/* libusb.c - USB endpoint wrapper using libusb. */

#include <flipper.h>
#include <libusb.h>

struct _lf_libusb_context {
	struct libusb_device_handle *handle;
	struct libusb_context *context;
};

int lf_libusb_read(struct _lf_device *device, void *dst, uint32_t length) {
	lf_assert(device, E_NULL, "invalid device");

	struct _lf_libusb_context *context = (struct _lf_libusb_context *)device->_ep_ctx;
	lf_assert(context, E_NULL, "invalid context");

    int e;

	while (length) {
        int len = (length > BULK_IN_SIZE) ? BULK_IN_SIZE : length;
        int actual = 0;

        e = libusb_bulk_transfer(context->handle, BULK_IN_ENDPOINT, dst, len, &actual, LF_USB_TIMEOUT_MS);
		lf_assert(e, E_LIBUSB, "read transfer failed");

		dst += actual;
		length -= actual;
	}

	return lf_success;
fail:
	return lf_error;
}

int lf_libusb_write(struct _lf_device *device, void *src, uint32_t length) {
	lf_assert(device, E_NULL, "invalid device");

	struct _lf_libusb_context *context = (struct _lf_libusb_context *)device->_ep_ctx;
	lf_assert(context, E_NULL, "invalid context");

    int e;

	while (length) {
        int len = (length > BULK_OUT_SIZE) ? BULK_OUT_SIZE : length;
        int actual = 0;

        e = libusb_bulk_transfer(context->handle, BULK_OUT_ENDPOINT, src, len, &actual, LF_USB_TIMEOUT_MS);
		lf_assert(e, E_LIBUSB, "write transfer failed");

		src += actual;
		length -= actual;
	}

	return lf_success;
fail:
	return lf_error;
}

int lf_libusb_release(struct _lf_device *device) {
	lf_assert(device, E_NULL, "invalid device");

	struct _lf_libusb_context *context = (struct _lf_libusb_context *)device->_ep_ctx;
	lf_assert(context, E_NULL, "invalid context");
	libusb_close(context->handle);
	libusb_exit(context->context);

	return lf_success;
fail:
	return lf_error;
}

struct _lf_ll *lf_libusb_devices_for_vid_pid(uint16_t vid, uint16_t pid) {
	struct libusb_context *context = NULL;
	struct libusb_device **libusb_devices = NULL;
	struct libusb_device *libusb_device = NULL;
	struct libusb_device_descriptor descriptor;
	struct _lf_ll *devices = NULL;
    int e;

    e = libusb_init(&context);
	lf_assert(e == 0, E_LIBUSB, "Failed to initialize libusb. Reboot and try again.");

	size_t device_count = libusb_get_device_list(context, &libusb_devices);
	for (size_t i = 0; i < device_count; i ++) {
		libusb_device = libusb_devices[i];

		lf_assert(libusb_get_device_descriptor(libusb_device, &descriptor), E_LIBUSB, "Failed to obtain descriptor for device.");

		if (descriptor.idVendor == vid && descriptor.idProduct == pid) {
			struct _lf_device *device = lf_device_create(lf_libusb_read, lf_libusb_write, lf_libusb_release);
			lf_assert(device, E_ENDPOINT, "Failed to create device");
			device->_ep_ctx = calloc(1, sizeof(struct _lf_network_context));

			struct _lf_libusb_context *context = (struct _lf_libusb_context *)device->_ep_ctx;
			lf_assert(context, E_NULL, "Failed to allocate memory for context");

            e = libusb_open(libusb_device, &(context->handle));
			lf_assert(e == 0, E_NO_DEVICE, "Could not find any devices connected via USB. Ensure that a device is connected.");

            e = libusb_claim_interface(context->handle, FMR_INTERFACE);
			lf_assert(e == 0, E_LIBUSB, "Failed to claim interface on attached device. Please quit any other programs using your device.");

            e = lf_ll_append(&devices, device, lf_device_release);
			lf_assert(e == 0, E_NULL, "Failed to append to device list.");
		}
	}

	return devices;

fail:
    /* TODO: ensure that libusb state is released properly on failure */
	lf_ll_release(&devices);
	libusb_free_device_list(libusb_devices, 1);
	libusb_exit(context);
	return NULL;
}
