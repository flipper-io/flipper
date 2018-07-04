/* libusb.c - USB endpoint wrapper using libusb. */

#include <flipper.h>
#include "usb.h"
#include <libusb.h>

#define LF_USB_TIMEOUT_MS 200

struct _lf_libusb_context {
	struct libusb_device_handle *handle;
	struct libusb_context *context;
    uint8_t in_sz, out_sz;
    uint8_t in, out;
};

int lf_libusb_read(struct _lf_device *device, void *dst, uint32_t length) {

	int len;
	int actual;
	int e;

	lf_assert(device, E_NULL, "invalid device");

	struct _lf_libusb_context *ctx = (struct _lf_libusb_context *)device->_ep_ctx;
	lf_assert(ctx, E_NULL, "invalid context");

	while (length) {
        len = (length > ctx->in_sz) ? ctx->in_sz : length;

		lf_debug("Reading %i from libusb.", length);
        e = libusb_bulk_transfer(ctx->handle, ctx->in, dst, len, &actual, LF_USB_TIMEOUT_MS);
		lf_assert(e == 0, E_LIBUSB, "read transfer failed (%s)", libusb_error_name(e));

		dst += len;
		length -= len;
	}

	return lf_success;
fail:
	return lf_error;
}

int lf_libusb_write(struct _lf_device *device, void *src, uint32_t length) {

	int len;
	int actual;
	int e;

	lf_assert(device, E_NULL, "invalid device");

	struct _lf_libusb_context *ctx = (struct _lf_libusb_context *)device->_ep_ctx;
	lf_assert(ctx, E_NULL, "invalid context");

	while (length) {
        len = (length > ctx->out_sz) ? ctx->out_sz : length;

		lf_debug("Sending %i through libusb.", length);
        e = libusb_bulk_transfer(ctx->handle, ctx->out, src, len, &actual, LF_USB_TIMEOUT_MS);
		lf_assert(e == 0, E_LIBUSB, "write transfer failed (%s)", libusb_error_name(e));

		src += len;
		length -= len;
	}

	return lf_success;
fail:
	return lf_error;
}

int lf_libusb_release(struct _lf_device *device) {
	lf_assert(device, E_NULL, "invalid device");

	struct _lf_libusb_context *ctx = (struct _lf_libusb_context *)device->_ep_ctx;
	lf_assert(ctx, E_NULL, "invalid context");

	libusb_close(ctx->handle);
	libusb_exit(ctx->context);

	return lf_success;
fail:
	return lf_error;
}

/*
   Returns a list of all of the Flipper devices attached over USB.

   If a USB device has the Flipper VID, its control endpoint is claimed.
*/

struct _lf_ll *lf_libusb_get_devices(void) {

	struct libusb_context *context = NULL;
	struct libusb_device **libusb_devices = NULL;
	struct libusb_device_descriptor descriptor;
	struct _lf_ll *devices = NULL;
	struct _lf_device *device = NULL;
	size_t count = 0;
    int e;

    e = libusb_init(&context);
	lf_assert(e == 0, E_LIBUSB, "failed to initialize libusb");

	count = libusb_get_device_list(context, &libusb_devices);

	for (size_t i = 0; i < count; i ++) {

		struct libusb_device *libusb_device = libusb_devices[i];

		e = libusb_get_device_descriptor(libusb_device, &descriptor);
		lf_assert(e == 0, E_LIBUSB, "failed to obtain descriptor for device");

		/* check that the device's VID matches our VID */
		if (descriptor.idVendor == FLIPPER_USB_VENDOR_ID) {
			device = lf_device_create(lf_libusb_read, lf_libusb_write, lf_libusb_release);
			lf_assert(device, E_ENDPOINT, "failed to create device");

			device->_ep_ctx = calloc(1, sizeof(struct _lf_libusb_context));
			lf_assert(context, E_NULL, "failed to allocate memory for context");

			struct _lf_libusb_context *ctx = (struct _lf_libusb_context *)device->_ep_ctx;
			ctx->context = context;

            e = libusb_open(libusb_device, &(ctx->handle));
			lf_assert(e == 0, E_NO_DEVICE, "Could not find any devices connected via USB. Ensure that a device is connected.");

            e = libusb_claim_interface(ctx->handle, FLIPPER_USB_CONTROL_INTERFACE);
			lf_assert(e == 0, E_LIBUSB, "Failed to claim interface on attached device. Please quit any other programs using your device.");

			lf_assert(lf_ll_append(&devices, device, lf_device_release), E_NULL, "failed to append to device list");
		}
	}

	return devices;
fail:

	lf_ll_release(&devices);
	libusb_free_device_list(libusb_devices, 1);
	libusb_exit(context);

	return NULL;
}
