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

#include <flipper.h>
#include <flipper/posix/usb.h>
#include <flipper/posix/network.h>
#include <flipper/atmegau2/atmegau2.h>

void sam_reset(void) {
	struct _lf_device *device = lf_get_selected();
	carbon_select_u2(device);
	/* reset low (active) */
	gpio_write(0, (1 << SAM_RESET_PIN));
	usleep(10000);
	/* reset high (inactive) */
	gpio_write((1 << SAM_RESET_PIN), 0);
	lf_select(device);
}

int sam_enter_dfu(void) {
	struct _lf_device *device = lf_get_selected();
	carbon_select_u2(device);
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
	carbon_select_u2(device);
	/* power off, reset low */
	gpio_write(0, (1 << SAM_POWER_PIN) | (1 << SAM_RESET_PIN));
	lf_select(device);
	return lf_success;
}

int sam_on(void) {
	struct _lf_device *device = lf_get_selected();
	carbon_select_u2(device);
	/* power on, reset high */
	gpio_write((1 << SAM_POWER_PIN) | (1 << SAM_RESET_PIN), 0);
	lf_select(device);
	return lf_success;
}

struct _lf_device *carbon_select_4s(struct _lf_device *device) {
	lf_assert(device, failure, E_NULL, "No device provided to '%s'.", __PRETTY_FUNCTION__);
	struct _carbon_context *ctx = (struct _carbon_context *)device->_dev_ctx;
	lf_select(ctx->_4s);
	return ctx->_4s;
failure:
	return NULL;
}

struct _lf_device *carbon_select_u2(struct _lf_device *device) {
	lf_assert(device, failure, E_NULL, "No device provided to '%s'.", __PRETTY_FUNCTION__);
	struct _carbon_context *ctx = (struct _carbon_context *)device->_dev_ctx;
	lf_select(ctx->_u2);
	return ctx->_u2;
failure:
	return NULL;
}

int atsam4s_read(struct _lf_device *device, void *dst, uint32_t length) {
	struct _lf_device *prev = lf_get_selected();
	lf_assert(device, failure, E_NULL, "No device provided to '%s'.", __PRETTY_FUNCTION__);
	carbon_select_u2(device);
	size_t size = 128;
	size_t packets = lf_ceiling(length, size);
	for (size_t i = 0; i < packets; i ++) {
		size_t len = (length > size) ? size : length;
		int _e = uart0_read(dst, len);
		if (_e) {
			lf_select(prev);
			return _e;
		}
		dst += size;
		length -= size;
	}
	lf_select(prev);
	return lf_success;
failure:
	return lf_error;
}

int atsam4s_write(struct _lf_device *device, void *src, uint32_t length) {
	struct _lf_device *prev = lf_get_selected();
	lf_assert(device, failure, E_NULL, "No device provided to '%s'.", __PRETTY_FUNCTION__);
	carbon_select_u2(device);
	size_t size = 128;
	size_t packets = lf_ceiling(length, size);
	for (size_t i = 0; i < packets; i ++) {
		size_t len = (length > size) ? size : length;
		int _e = uart0_write(src, len);
		if (_e) {
			lf_select(prev);
			return _e;
		}
		src += size;
		length -= size;
	}
	lf_select(prev);
	return lf_success;
failure:
	return lf_error;
}

int atsam4s_release(struct _lf_device *device) {
	return lf_success;
}

int carbon_attach_u2s(const void *__u2, void *_unused) {
	struct _lf_device *_u2 = (struct _lf_device *)__u2;
	_u2->_dev_ctx = calloc(1, sizeof(struct _carbon_context));
	struct _carbon_context *_u2_context = (struct _carbon_context *)_u2->_dev_ctx;
	lf_assert(_u2_context, failure, E_NULL, "Failed to allocate memory for context in '%s'.", __PRETTY_FUNCTION__);

	struct _lf_device *_4s = lf_device_create(atsam4s_read, atsam4s_write, atsam4s_release);
	lf_assert(_4s, failure, E_NULL, "Failed to create 4s subdevice in '%s'.", __PRETTY_FUNCTION__);

	_4s->_dev_ctx = calloc(1, sizeof(struct _carbon_context));
	struct _carbon_context *_4s_context = (struct _carbon_context *)_4s->_dev_ctx;
	lf_assert(_4s_context, failure, E_NULL, "Failed to allocate memory for context in '%s'.", __PRETTY_FUNCTION__);

	_u2_context->_u2 = _4s_context->_u2 = _u2;
	_u2_context->_4s = _4s_context->_4s = _4s;

	return lf_attach(_4s);
failure:
	return lf_error;
}

#warning Carbon is broken until carbon context is reinstated.

/* Attaches to all of the Carbon devices available on the system. */
int carbon_attach(void) {
	struct _lf_ll *_u2s = lf_libusb_devices_for_vid_pid(CARBON_USB_VENDOR_ID, CARBON_USB_PRODUCT_ID);
	if (!_u2s) return lf_error;
	return lf_ll_apply_func(_u2s, carbon_attach_u2s, NULL);
}

struct _lf_device *carbon_attach_hostname(char *hostname) {
	struct _lf_device *device = lf_network_device_for_hostname(hostname);
	lf_assert(device, failure, E_NO_DEVICE, "Failed to find Carbon device with hostname '%s'.", hostname);
	lf_attach(device);
	return device;

failure:
	return NULL;
}
