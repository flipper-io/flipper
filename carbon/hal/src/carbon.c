/* Hardware abstraction layer around the ATMega16U2 USB bridge device. */

/*
 * Flipper: Carbon Edition incorperates a multi-cpu harware architecture.
 * The atmeaga16u2 device is used to handle USB interactions with the host,
 * while the atsam4s16b is used to perfrom computations and interact with
 * device peripherals. Following the block diagram below, the 4S device is
 * isolated from the host, only able to send and receive communications
 * via its uart0 hardware bus. This HAL provides access to the 4S device using
 * libusb and the atmega16u2 device as a Flipper Message Runtime bridge to the
 * 4S hardware. This is accomplished by creating an FMR endpoint using the U2's
 * uart0 bus, and using it to route FMR packets to the 4S.
 */

/*
 *   Hardware block diagram for this HAL.
 *
 *                        ------                    ------
 *   PC < -- libusb -- > |  U2  |  < -- uart0 -- > |  4S  |
 *                        ------                    ------
 */

#include "flipper.h"

#include <posix/usb.h>
#include <posix/network.h>
#include <libusb.h>

#include "atmegau2.h"

void sam_reset(void) {
	struct _lf_device *device = lf_get_selected();
//	carbon_select_u2(device);

	/* reset low (active) */
	gpio_write(0, (1 << SAM_RESET_PIN));
	usleep(10000);
	/* reset high (inactive) */
	gpio_write((1 << SAM_RESET_PIN), 0);

	lf_select(device);
}

int sam_enter_dfu(void) {
	struct _lf_device *device = lf_get_selected();
//	carbon_select_u2(device);

	/* erase high, reset low (active) */
	gpio_write((1 << SAM_ERASE_PIN), (1 << SAM_RESET_PIN));
	/* Wait for chip to erase. */
	usleep(8000000);
	/* erase low, reset high (inactive) */
	gpio_write((1 << SAM_RESET_PIN), (1 << SAM_ERASE_PIN));

	lf_select(device);
	return lf_success;
}

int sam_off(void) {
	struct _lf_device *device = lf_get_selected();
//	carbon_select_u2(device);

	/* power off, reset low */
	gpio_write(0, (1 << SAM_POWER_PIN) | (1 << SAM_RESET_PIN));

	lf_select(device);
	return lf_success;
}

int sam_on(void) {
	struct _lf_device *device = lf_get_selected();
//	carbon_select_u2(device);

	/* power on, reset high */
	gpio_write((1 << SAM_POWER_PIN) | (1 << SAM_RESET_PIN), 0);

	lf_select(device);
	return lf_success;
}

int atsam4s_read(struct _lf_device *device, void *dst, uint32_t length) {
	struct _lf_device *prev = lf_get_selected();
	lf_assert(device, E_NULL, "invalid device");
//	carbon_select_u2(device);

	size_t size = 128;
	while (length) {
		size_t len = (length > size) ? size : length;
		int e = uart0_read(dst, len);
		if (e) {
			lf_select(prev);
			return e;
		}
		dst += size;
		length -= size;
	}

	lf_select(prev);

	return lf_success;
fail:
	return lf_error;
}

int atsam4s_write(struct _lf_device *device, void *src, uint32_t length) {
	struct _lf_device *prev = lf_get_selected();
	lf_assert(device, E_NULL, "invalid device");
//	carbon_select_u2(device);

	size_t size = 128;
	while (length) {
		size_t len = (length > size) ? size : length;
		int e = uart0_write(src, len);
		if (e) {
			lf_select(prev);
			return e;
		}
		src += size;
		length -= size;
	}

	lf_select(prev);

	return lf_success;
fail:
	return lf_error;
}

int atsam4s_release(struct _lf_device *device) {
	return lf_success;
}

int carbon_attach_u2s(const void *__u2, void *_unused) {
	struct _lf_device *_u2 = (struct _lf_device *)__u2;
	_u2->_dev_ctx = calloc(1, sizeof(struct _carbon_context));
	struct _carbon_context *_u2_context = (struct _carbon_context *)_u2->_dev_ctx;
	lf_assert(_u2_context, E_NULL, "Failed to allocate memory for context");

	struct _lf_device *_4s = lf_device_create(atsam4s_read, atsam4s_write, atsam4s_release);
	lf_assert(_4s, E_NULL, "Failed to create 4s subdevice");

	_4s->_dev_ctx = calloc(1, sizeof(struct _carbon_context));
	struct _carbon_context *_4s_context = (struct _carbon_context *)_4s->_dev_ctx;
	lf_assert(_4s_context, E_NULL, "Failed to allocate memory for context");

	_u2_context->_u2 = _4s_context->_u2 = _u2;
	_u2_context->_4s = _4s_context->_4s = _4s;

	return lf_attach(_4s);
fail:
	return lf_error;
}

#warning Carbon is broken until carbon context is reinstated.

/* Attaches to all of the Carbon devices available on the system. */
int carbon_attach(void) {
    /* TODO: Set in, out, in_sz, out_sz */
	struct _lf_ll *_u2s = lf_libusb_devices_for_vid_pid(FLIPPER_USB_VENDOR_ID, USB_PRODUCT_ID);
	if (!_u2s) return lf_error;
	return lf_ll_apply_func(_u2s, carbon_attach_u2s, NULL);
}

struct _lf_device *carbon_attach_hostname(char *hostname) {
	struct _lf_device *device = lf_network_device_for_hostname(hostname);
	lf_assert(device, E_NO_DEVICE, "Failed to find Carbon device with hostname '%s'.", hostname);
	lf_attach(device);
	return device;

fail:
	return NULL;
}
