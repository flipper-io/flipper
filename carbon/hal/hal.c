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
#include <flipper/libflipper.h>
#include <flipper/carbon.h>
#include <flipper/posix/libusb.h>
#include <flipper/posix/network.h>
#include <flipper/atmegau2/atmegau2.h>
//#include <flipper/atsam4s/atsam4s.h>

/* Selects a carbon device. */
int carbon_select(struct _lf_device *device) {
	LF_MODULE_SET_DEVICE_AND_ID(_adc, device, _led_id);
	LF_MODULE_SET_DEVICE_AND_ID(_button, device, _button_id);
	//LF_MODULE_SET_DEVICE_AND_ID(_dac, device, _dac_id);
	LF_MODULE_SET_DEVICE_AND_ID(_fs, device, _fs_id);
	LF_MODULE_SET_DEVICE_AND_ID(_gpio, device, _gpio_id);
	//LF_MODULE_SET_DEVICE_AND_ID(_i2c, device, _i2c_id);
	LF_MODULE_SET_DEVICE_AND_ID(_led, device, _led_id);
	//LF_MODULE_SET_DEVICE_AND_ID(_pwm, device, _pwm_id);
	//LF_MODULE_SET_DEVICE_AND_ID(_rtc, device, _rtc_id);
	//LF_MODULE_SET_DEVICE_AND_ID(_spi, device, _spi_id);
	//LF_MODULE_SET_DEVICE_AND_ID(_swd, device, _swd_id);
	//LF_MODULE_SET_DEVICE_AND_ID(_task, device, _task_id);
	//LF_MODULE_SET_DEVICE_AND_ID(_temp, device, _temp_id);
	//LF_MODULE_SET_DEVICE_AND_ID(_timer, device, _timer_id);
	LF_MODULE_SET_DEVICE_AND_ID(_uart0, device, _uart0_id);
	//LF_MODULE_SET_DEVICE_AND_ID(_usart, device, _usart_id);
	//LF_MODULE_SET_DEVICE_AND_ID(_usb, device, _usb_id);
	LF_MODULE_SET_DEVICE_AND_ID(_wdt, device, _wdt_id);
	return lf_success;
}

struct _lf_device *carbon_attach_endpoint(struct _lf_endpoint *endpoint) {
	struct _lf_device *device = lf_device_create(endpoint, carbon_select);
	lf_assert(device, failure, E_NULL, "Failed to create new Carbon device.");
	device->selector = carbon_select;
	lf_attach(device);
	return device;
failure:
	return NULL;
}

void carbon_attach_endpoint_applier(const void *_endpoint, void *_other) {
	struct _lf_endpoint *endpoint = (struct _lf_endpoint *)_endpoint;
	carbon_attach_endpoint(endpoint);
}

/* Attaches to all of the Carbon devices available on the system. */
int carbon_attach(void) {
	/* Obtains a list of endpoints for all Carbon devices attached to the system. */
	struct _lf_ll *endpoints = lf_libusb_endpoints_for_vid_pid(CARBON_USB_VENDOR_ID, CARBON_USB_PRODUCT_ID);
	if (!endpoints) return lf_error;
	lf_ll_apply_func(endpoints, carbon_attach_endpoint_applier, NULL);
	return lf_success;
}

struct _lf_device *carbon_attach_hostname(char *hostname) {
	struct _lf_endpoint *endpoint = lf_network_endpoint_for_hostname(hostname);
	lf_assert(endpoint, failure, E_NO_DEVICE, "Failed to find Carbon device using hostname '%s'.", hostname);
	return carbon_attach_endpoint(endpoint);
failure:
	return NULL;
}

struct _carbon_context {
	/* Microcontroller that handles USB interaction. (U2) */
	struct _lf_device *mcu;
	/* Microprocessor that handles code execution. (4S) */
	struct _lf_device *mpu;
};

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
