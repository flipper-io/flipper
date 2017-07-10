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
#include <flipper/atmegau2/atmegau2.h>

struct _carbon_record {
	/* Microcontroller that handles USB interaction. (U2) */
	struct _lf_device *mcu;
	/* Microprocessor that handles code execution. (4S) */
	struct _lf_device *mpu;
};

int carbon_select(struct _lf_device *device);

void *carbon_create(void *_endpoint, void *_other) {
	/* The MCU's endpoint. */
	struct _lf_endpoint *mcu_ep = _endpoint;
	struct _lf_device *mcu = lf_device_create(mcu_ep);
	lf_load_configuration(mcu);
	struct _lf_endpoint *mpu_ep = lf_endpoint_create(&uart0, NULL);
	struct _lf_device *mpu = lf_device_create(mpu_ep);
	lf_load_configuration(mpu);
	return NULL;
}

/* The Carbon architecture is interesting because we actually have to attach
 * two flipper devices, the U2 and the 4S. We need libflipper to understand
 * how to interact with each of these devices as well as to forward calls to
 * the standard modules to the appropriate hardware that implements them.
 *
 * The way that this is accomplished is a list of U2 devices is first obtained
 * from libusb, and then each device is mutated.
 *
 */

int carbon_attach(void) {
	struct _lf_ll *endpoints = lf_libusb_endpoints_for_vid_pid(CARBON_USB_VENDOR_ID, CARBON_USB_PRODUCT_ID);
	lf_assert(endpoints, failure, E_NULL, "Failed to claim any Carbon endpoints.");
	/* Make each endpoint into a Carbon device. */
	lf_ll_apply_func(endpoints, NULL, carbon_create);
	return lf_success;
failure:
	return lf_error;
}

/* Selects a carbon device. */
int carbon_select(struct _lf_device *device) {
	return lf_success;
}

/* ----------- OLD API ------------ */
/* ~ Declare the prototypes for all functions exposed by this driver. ~ */

int lf_bridge_configure();
uint8_t lf_bridge_ready(struct _lf_endpoint *this);
void lf_bridge_put(struct _lf_endpoint *this, uint8_t byte);
uint8_t lf_bridge_get(struct _lf_endpoint *this);
int lf_bridge_push(struct _lf_endpoint *this, void *source, lf_size_t length);
int lf_bridge_pull(struct _lf_endpoint *this, void *destination, lf_size_t length);
int lf_bridge_destroy(struct _lf_endpoint *this);


struct _lf_endpoint lf_atmegau2_uart0_prototype = {
	lf_bridge_configure,
	lf_bridge_ready,
	lf_bridge_put,
	lf_bridge_get,
	lf_bridge_push,
	lf_bridge_pull,
	lf_bridge_destroy
};

struct _lf_bridge_record {
	/* Create the bridge device. */
	struct _lf_device atmega16u2;
	/* A pointer to the device above. */
	struct _lf_device *_atmega16u2;
	/* Create the bridge module. */
	struct _lf_module _uart0_bridge;
};

#define LF_ASSIGN_MODULE(module, id) module.device = record -> _atmega16u2; module.index = id;

int lf_bridge_configure(struct _lf_device *device) {
	/* Ensure that the device pointer is valid. */
	if (!device) {
		lf_error_raise(E_NULL, error_message("No device or endpoint record provided for libusb configuration. Reattach your device and try again."));
		return lf_error;
	}
	/* Allocate memory for the bridge record if it has not yet been allocated. */
	if (!(device -> endpoint -> record)) {
		device -> endpoint -> record = calloc(1, sizeof(struct _lf_bridge_record));
		if (!(device -> endpoint -> record)) {
			lf_error_raise(E_MALLOC, error_message("Failed to allocate the memory needed to create a bridge endpoint record."));
			goto failure;
		}
	}
	struct _lf_bridge_record *record = device -> endpoint -> record;
	/* Set the bridge device pointer. */
	record -> _atmega16u2 = &(record -> atmega16u2);
	/* Copy the initial configuration. (name and identifier) */
	memcpy(&(record -> atmega16u2.configuration), &(device -> configuration), sizeof(struct _lf_configuration));

	/* Set the endpoint. */
	// record -> atmega16u2.endpoint = &lf_libusb_ep;

	/* Configure the endpoint. */
	record -> atmega16u2.endpoint -> configure(record -> atmega16u2.endpoint, record -> _atmega16u2);
	/* Assign the functionality of the device specific modules to this device. */
	LF_ASSIGN_MODULE(_button, _button_id);
	LF_ASSIGN_MODULE(_gpio, _gpio_id);
	LF_ASSIGN_MODULE(_fs, _fs_id);
	LF_ASSIGN_MODULE(_led, _led_id);
	LF_ASSIGN_MODULE(_uart0, _uart0_id);
	LF_ASSIGN_MODULE(_wdt, _wdt_id);
	/* Set the bridge module's index. */
	record -> _uart0_bridge.index = _uart0_id;
	/* Set the bridge module's device pointer pointer. */
	record -> _uart0_bridge.device = record -> _atmega16u2;
	/* Set the bridged device's attributes. */
	device -> configuration.attributes = (lf_device_32bit | lf_device_little_endian);
	return lf_success;
failure:
	lf_bridge_destroy(device -> endpoint);
	return lf_error;
}

uint8_t lf_bridge_ready(struct _lf_endpoint *this) {
	struct _lf_bridge_record *record = this -> record;
	return lf_invoke(&(record -> _uart0_bridge), _uart0_ready, NULL);
}

void lf_bridge_put(struct _lf_endpoint *this, uint8_t byte) {
	struct _lf_bridge_record *record = this -> record;
	lf_invoke(&(record -> _uart0_bridge), _uart0_put, fmr_args(fmr_infer(byte)));
}

uint8_t lf_bridge_get(struct _lf_endpoint *this) {
	struct _lf_bridge_record *record = this -> record;
	return lf_invoke(&(record -> _uart0_bridge), _uart0_get, NULL);
}

#define CHUNK_SIZE 128

int lf_bridge_push(struct _lf_endpoint *this, void *source, lf_size_t length) {
	struct _lf_bridge_record *record = this -> record;
	int _e = lf_success;
	for (int i = 0; i < length / CHUNK_SIZE; i ++) {
		_e = lf_push(&(record -> _uart0_bridge), _uart0_push, source, CHUNK_SIZE, NULL);
		if (_e < lf_success) {
			return _e;
		}
		length -= CHUNK_SIZE;
		source += CHUNK_SIZE;
	}
	if (length) {
		_e = lf_push(&(record -> _uart0_bridge), _uart0_push, source, length, NULL);
	}
	return _e;
}

int lf_bridge_pull(struct _lf_endpoint *this, void *destination, lf_size_t length) {
	struct _lf_bridge_record *record = this -> record;
	int _e = lf_success;
	for (int i = 0; i < length / CHUNK_SIZE; i ++) {
		_e = lf_pull(&(record -> _uart0_bridge), _uart0_pull, destination, CHUNK_SIZE, fmr_args(fmr_int32(0xff)));
		if (_e < lf_success) {
			return _e;
		}
		length -= CHUNK_SIZE;
		destination += CHUNK_SIZE;
	}
	if (length) {
		_e = lf_pull(&(record -> _uart0_bridge), _uart0_pull, destination, length, fmr_args(fmr_int32(0xff)));
	}
	return _e;
}

int lf_bridge_destroy(struct _lf_endpoint *this) {
	struct _lf_bridge_record *record = this -> record;
	/* If the record was allocated, release it .*/
	if (record) {
		struct _lf_endpoint *endpoint = record -> atmega16u2.endpoint;
		/* If the endpoint was allocated, destroy it. */
		if (endpoint) {
			endpoint -> destroy(endpoint);
		}
		free(record);
	}
	return lf_success;
}
