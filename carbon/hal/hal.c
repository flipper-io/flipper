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
	carbon_select_u2(lf_get_current_device());
	/* reset low (active) */
	gpio_write(0, (1 << SAM_RESET_PIN));
	usleep(10000);
	/* reset high (inactive) */
	gpio_write((1 << SAM_RESET_PIN), 0);
	carbon_select_4s(lf_get_current_device());
}

int sam_enter_dfu(void) {
	carbon_select_u2(lf_get_current_device());
	/* erase high, reset low (active) */
	gpio_write((1 << SAM_ERASE_PIN), (1 << SAM_RESET_PIN));
	/* Wait for chip to erase. */
	usleep(8000000);
	/* erase low, reset high (inactive) */
	gpio_write((1 << SAM_RESET_PIN), (1 << SAM_ERASE_PIN));
	carbon_select_4s(lf_get_current_device());
	return lf_success;
}

int sam_off(void) {
	carbon_select_u2(lf_get_current_device());
	/* power off, reset low */
	gpio_write(0, (1 << SAM_POWER_PIN) | (1 << SAM_RESET_PIN));
	carbon_select_4s(lf_get_current_device());
	return lf_success;
}

int sam_on(void) {
	carbon_select_u2(lf_get_current_device());
	/* power on, reset high */
	gpio_write((1 << SAM_POWER_PIN) | (1 << SAM_RESET_PIN), 0);
	carbon_select_4s(lf_get_current_device());
	return lf_success;
}

struct _lf_device *carbon_select_4s(struct _lf_device *device) {
	lf_assert(device, failure, E_NULL, "No device provided to '%s'.", __PRETTY_FUNCTION__);
	struct _carbon_context *ctx = (struct _carbon_context *)device->_ctx;
	lf_select(ctx->_4s);
	return ctx->_4s;
failure:
	return NULL;
}

struct _lf_device *carbon_select_u2(struct _lf_device *device) {
	lf_assert(device, failure, E_NULL, "No device provided to '%s'.", __PRETTY_FUNCTION__);
	struct _carbon_context *ctx = (struct _carbon_context *)device->_ctx;
	lf_select(ctx->_u2);
	return ctx->_u2;
failure:
	return NULL;
}

int uart0_bridge_configure(struct _lf_device *device, void *_configuration) {
	lf_assert(device, failure, E_NULL, "No device provided to '%s'.", __PRETTY_FUNCTION__);
	carbon_select_u2(device);
	int _e = uart0_configure();
	carbon_select_4s(device);
	return _e;
failure:
	return lf_error;
}

bool uart0_bridge_ready(struct _lf_device *device) {
	lf_assert(device, failure, E_NULL, "No device provided to '%s'.", __PRETTY_FUNCTION__);
	carbon_select_u2(device);
	int _e = uart0_ready();
	carbon_select_4s(device);
	return _e;
failure:
	return false;
}

int uart0_bridge_push(struct _lf_device *device, void *source, lf_size_t length) {
	lf_assert(device, failure, E_NULL, "No device provided to '%s'.", __PRETTY_FUNCTION__);
	carbon_select_u2(device);
	lf_size_t size = 128;
	lf_size_t packets = lf_ceiling(length, size);
	for (lf_size_t i = 0; i < packets; i ++) {
		lf_size_t len = (length > size) ? size : length;
		int _e = uart0_push(source, len);
		if (_e) {
			carbon_select_4s(device);
			return _e;
		}
		source += size;
		length -= size;
	}
	carbon_select_4s(device);
	return lf_success;
failure:
	return lf_error;
}

int uart0_bridge_pull(struct _lf_device *device, void *destination, lf_size_t length) {
	lf_assert(device, failure, E_NULL, "No device provided to '%s'.", __PRETTY_FUNCTION__);
	carbon_select_u2(device);
	lf_size_t size = 128;
	lf_size_t packets = lf_ceiling(length, size);
	for (lf_size_t i = 0; i < packets; i ++) {
		lf_size_t len = (length > size) ? size : length;
		int _e = uart0_pull(destination, len);
		if (_e) {
			carbon_select_4s(device);
			return _e;
		}
		destination += size;
		length -= size;
	}
	carbon_select_4s(device);
	return lf_success;
failure:
	return lf_error;
}

void carbon_attach_to_usb_endpoint_applier(const void *__u2_ep, void *_other) {
	/* Obtain the u2's endpoint. */
	struct _lf_endpoint *_u2_ep = (struct _lf_endpoint *)__u2_ep;
	/* Create the u2 sub-device. */
	struct _lf_device *_u2 = lf_device_create("u2", _u2_ep);
	/* Create the 4s' endpoint using the u2's uart0 endpoint as a bridge. */
	struct _lf_endpoint *_4s_ep = lf_endpoint_create(uart0_bridge_configure, uart0_bridge_ready, uart0_bridge_push, uart0_bridge_pull, NULL, 0);
	/* Create the 4s sub-device. */
	struct _lf_device *_4s = lf_device_create("4s", _4s_ep);
	/* Set the 4s's context. */
	struct _carbon_context *context = calloc(1, sizeof(struct _carbon_context));
	lf_assert(context, failure, E_MALLOC, "Failed to allocate memory for carbon context.");
	/* Set the carbon's u2 and 4s sub-devices. */
	context->_u2 = _u2;
	context->_4s = _4s;
	/* Write back the contexts. */
	_u2->_ctx = context;
	_4s->_ctx = context;
	/* Attach to the new carbon device. */
	lf_attach(_4s);
failure:
	return;
}

/* Attaches to all of the Carbon devices available on the system. */
int carbon_attach(void) {
	/* Obtains a list of endpoints for all Carbon devices attached to the system. */
	struct _lf_ll *endpoints = lf_libusb_endpoints_for_vid_pid(CARBON_USB_VENDOR_ID, CARBON_USB_PRODUCT_ID);
	if (!endpoints) return lf_error;
	lf_ll_apply_func(endpoints, carbon_attach_to_usb_endpoint_applier, NULL);
	return lf_success;
}

struct _lf_device *carbon_attach_hostname(char *hostname) {
	struct _lf_endpoint *endpoint = lf_network_endpoint_for_hostname(hostname);
	lf_assert(endpoint, failure, E_NO_DEVICE, "Failed to find Carbon device using hostname '%s'.", hostname);
	struct _lf_device *carbon = lf_device_create("carbon", endpoint);
	lf_attach(carbon);
	return carbon;
failure:
	return NULL;
}
