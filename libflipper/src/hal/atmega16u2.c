/* Hardware abstraction layer around the ATMega16U2 USB bridge device. */

/*
 * Flipper: Carbon Edition incorperates a multi-cpu harware architecture.
 * The atmeaga16u2 device is used to handle USB interactions with the host,
 * while the atsam4s16b is used to perfrom computations and interact with
 * device peripherals. Following the block diagram below, the 4S device is
 * isolated from the host, only able to send and recieve communications
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
#include <flipper/flipper.h>
#include <flipper/modules.h>
#include <platforms/posix.h>

/* Declare any standard modules with functionality specific to this device. */
extern struct _lf_module _cpu;
extern struct _lf_module _uart0;

struct _lf_endpoint lf_bridge_ep = {
    lf_bridge_configure,
    lf_bridge_ready,
    lf_bridge_put,
    lf_bridge_get,
    lf_bridge_push,
    lf_bridge_pull,
    lf_bridge_destroy,
    NULL
};

struct _lf_bridge_record {
    /* Create the bridge device. */
    struct _lf_device atmega16u2;
    /* A pointer to the device above. */
    struct _lf_device *_atmega16u2;
    /* Create the bridge module. */
    struct _lf_module _uart0_bridge;
};

int lf_bridge_configure(struct _lf_endpoint *this) {
    if (!this) {
		error_raise(E_NULL, error_message("No endpoint record provided for libusb configuration. Reattach your device and try again."));
		return lf_error;
	}
	/* Allocate memory for the bridge record if it has not yet been allocated. */
	if (!(this -> record)) {
		this -> record = malloc(sizeof(struct _lf_bridge_record));
	}
	struct _lf_bridge_record *record = this -> record;
    /* Set the endpoint. */
    record -> atmega16u2.endpoint = &lf_libusb_ep;
    /* Configure the endpoint. */
    record -> atmega16u2.endpoint -> configure(record -> atmega16u2.endpoint);
    /* Set the bridge device pointer. */
    record -> _atmega16u2 = &(record -> atmega16u2);
    /* Assign the functionality of the device specific modules to this device. */
    _cpu.device = &(record -> _atmega16u2);
    _uart0.device = &(record -> _atmega16u2);
    /* Set the bridge module's index. */
    record -> _uart0_bridge.index = _uart0_id;
    /* Set the bridge module's device pointer pointer. */
    record -> _uart0_bridge.device = &(record -> _atmega16u2);
    return lf_success;
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

int lf_bridge_push(struct _lf_endpoint *this, void *source, lf_size_t length) {
    struct _lf_bridge_record *record = this -> record;
    return lf_push(&(record -> _uart0_bridge), _uart0_push, source, length, NULL);
}

int lf_bridge_pull(struct _lf_endpoint *this, void *destination, lf_size_t length) {
    struct _lf_bridge_record *record = this -> record;
    return lf_pull(&(record -> _uart0_bridge), _uart0_pull, destination, length, NULL);
}

int lf_bridge_destroy(struct _lf_endpoint *endpoint) {
    if (endpoint -> record) {
        free(endpoint -> record);
    }
    return lf_success;
}