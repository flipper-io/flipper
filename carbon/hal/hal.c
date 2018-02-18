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

#define __private_include__
#include <flipper.h>
#include <flipper/posix/usb.h>
#include <flipper/posix/network.h>

extern int carbon_select_atmegau2(struct _lf_device *device);
extern int carbon_select_atsam4s(struct _lf_device *device);

/* Selects a carbon device. */
int carbon_select(struct _lf_device *device) {
	lf_assert(device, failure, E_NULL, "NULL pointer provided when selecting device.");
	/* Obtain the carbon device's context. */
	struct _carbon_context *context = device->_ctx;
	lf_assert(context, failure, E_NULL, "No context for selected carbon device.");
	/* Select the modules on the 4s first. */
	carbon_select_atsam4s(device);
	/* Select the modules on the bridge next. */
	if (context->_4s) context->_u2->select(context->_u2);
	return lf_success;
failure:
	return lf_error;
}

int carbon_destroy(struct _lf_device *device);

struct _lf_device *carbon_attach_endpoint(struct _lf_endpoint *endpoint, struct _lf_device *_u2, struct _lf_device *_4s) {
	/* Create the parent carbon device. */
	struct _lf_device *carbon = lf_device_create(endpoint, carbon_select, carbon_destroy, sizeof(struct _carbon_context));
	/* Set the 4s's context. */
	struct _carbon_context *context = carbon->_ctx;
	/* Set the carbon's u2 and 4s sub-devices. */
	context->_u2 = _u2;
	context->_4s = _4s;
	/* Attach to the new carbon device. */
	lf_attach(carbon);
	return carbon;
}

int uart0_bridge_configure(struct _lf_endpoint *endpoint, void *_configuration) {
	return uart0_configure(FMR_BAUD, true);
}

bool uart0_bridge_ready(struct _lf_endpoint *endpoint) {
	return uart0_ready();
}

int uart0_bridge_push(struct _lf_endpoint *endpoint, void *source, lf_size_t length) {
	lf_size_t size = 128;
	lf_size_t packets = lf_ceiling(length, size);
	for (lf_size_t i = 0; i < packets; i ++) {
		lf_size_t len = (length > size) ? size : length;
		int err = uart0_push(source, len);
		if (err) return err;
		source += size;
		length -= size;
	}
	return lf_success;
}

int uart0_bridge_pull(struct _lf_endpoint *endpoint, void *destination, lf_size_t length) {
	lf_size_t size = 128;
	lf_size_t packets = lf_ceiling(length, size);
	for (lf_size_t i = 0; i < packets; i ++) {
		lf_size_t len = (length > size) ? size : length;
		int err = uart0_pull(destination, len);
		if (err) return err;
		destination += size;
		length -= size;
	}
	return lf_success;
}

void carbon_attach_to_usb_endpoint_applier(const void *__u2_ep, void *_other) {
	/* Obtain the u2's endpoint. */
	struct _lf_endpoint *_u2_ep = (struct _lf_endpoint *)__u2_ep;
	/* Create the u2 sub-device. */
	struct _lf_device *_u2 = lf_device_create(_u2_ep, carbon_select_atmegau2, NULL, 0);
	/* Create the 4s' endpoint using the u2's uart0 endpoint as a bridge. */
	struct _lf_endpoint *_4s_ep = lf_endpoint_create(uart0_bridge_configure, uart0_bridge_ready, uart0_bridge_push, uart0_bridge_pull, NULL, 0);
	/* Create the 4s sub-device. */
	struct _lf_device *_4s = lf_device_create(_4s_ep, carbon_select_atsam4s, NULL, 0);
	/* Attach to a carbon device over the 4s' endpoint. */
	carbon_attach_endpoint(_4s_ep, _u2, _4s);
}

/* Attaches to all of the Carbon devices available on the system. */
int carbon_attach(void) {
	/* Obtains a list of endpoints for all Carbon devices attached to the system. */
	struct _lf_ll *endpoints = lf_libusb_endpoints_for_vid_pid(CARBON_USB_VENDOR_ID, CARBON_USB_PRODUCT_ID);
	if (!endpoints) return lf_error;
	lf_ll_apply_func(endpoints, carbon_attach_to_usb_endpoint_applier, NULL);
	return lf_success;
}

int carbon_destroy(struct _lf_device *device) {
	if (device) {
		struct _carbon_context *context = (struct _carbon_context *)device->_ctx;
		lf_device_release(context->_u2);
		//lf_device_release(context->_4s);
	}
	return lf_success;
}

struct _lf_device *carbon_attach_hostname(char *hostname) {
	struct _lf_endpoint *endpoint = lf_network_endpoint_for_hostname(hostname);
	lf_assert(endpoint, failure, E_NO_DEVICE, "Failed to find Carbon device using hostname '%s'.", hostname);
	return carbon_attach_endpoint(endpoint, NULL, NULL);
failure:
	return NULL;
}

/* ----------- OLD API ------------ */

/* The Carbon architecture is interesting because we actually have to attach
 * two flipper devices, the U2 and the 4S. We need libflipper to understand
 * how to interact with each of these devices as well as to forward calls to
 * the standard modules to the appropriate hardware that implements them.
 *
 * The way that this is accomplished is a list of U2 devices is first obtained
 * from libusb, and then each device is mutated.
 *
 */
