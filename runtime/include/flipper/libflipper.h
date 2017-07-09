/*
 * libflipper.h
 *
 * Core API specifications for interaction with Flipper hardware.
 *
 */

#ifndef __libflipper_h__
#define __libflipper_h__

/* The current version of libflipper. */
#define LF_VERSION 0x0001

/* Configuration information for USB. */
#define CARBON_USB_MANUFACTURER    L"flipper.io"
#define CARBON_USB_PRODUCT         L"Flipper: Carbon"
#define CARBON_USB_VENDOR_ID       0x16C0
#define CARBON_USB_PRODUCT_ID      0x0480

/* If defined, imposes a timeout on USB transactions. */
#define __lf_usb_timeout__
#ifdef __lf_usb_timeout__
/* Must be between 1ms and 255ms. */
#define CARBON_USB_TIMEOUT_MS 255
#else
#define CARBON_USB_TIMEOUT_MS 0
#endif

/* NOTE: Summing the size parameters of each endpoints below should be less than or equal to 160. */
#define USB_IN_MASK            0x80
#define INTERRUPT_IN_ENDPOINT  (0x01 | USB_IN_MASK)
#define INTERRUPT_IN_SIZE      16
#define INTERRUPT_OUT_ENDPOINT 0x02
#define INTERRUPT_OUT_SIZE     16
#define BULK_IN_ENDPOINT       (0x03 | USB_IN_MASK)
#define BULK_IN_SIZE           64
#define BULK_OUT_ENDPOINT      0x04
#define BULK_OUT_SIZE          64

/* The name of the default device to attach to. */
#define LF_DEFAULT_NAME "flipper"

/* If defined, uses bulk for all USB transfers. */
#define __ALL_BULK__
/* If defined, prints debugging information about each packet. */
#define __lf_debug__

/* Computes the greatest integer from the result of the division of x by y. */
#define lf_ceiling(x, y) ((x + y - 1) / y)

/* Define bit manipulation macros. */
#define bit(b)								(0x01 << (b))
#define get_bit_from_port(b, p)				((p) & bit(b))
#define set_bit_in_port(b, p)				((p) |= bit(b))
#define set_bits_in_port_with_mask(p, m)	((p) |= (m))
#define clear_bit_in_port(b, p)				((p) &= ~(bit(b)))
#define clear_bits_in_port_with_mask(p, m)	((p) &= ~(m))
#define flip_bit_in_port(b, p)				((p) ^= bit(b))
#define flip_bits_in_port_with_mask(p, m)	((p) ^= (m))
#define lo(x) ((uint8_t)(x))
#define hi(x) ((uint8_t)(x >> 8))
#define lo16(x) ((uint16_t)(((uint32_t)(x))))
#define hi16(x) ((uint16_t)(((uint32_t)(x)) >> 16))
#define little(x) ((((uint16_t)(x)) << 8 ) | (((uint16_t)(x)) >> 8))
#define little32(x) ((((uint32_t)(x)) << 16 ) | (((uint32_t)(x)) >> 16))

#include <flipper/types.h>
#include <flipper/error.h>
#include <flipper/fmr.h>
#include <flipper/endpoint.h>
#include <flipper/ll.h>

/* Macros that quantify device attributes. */
#define lf_device_8bit (1 << 1)
#define lf_device_16bit (1 << 2)
#define lf_device_32bit (1 << 3)
#define lf_device_big_endian    1
#define lf_device_little_endian 0

/* Standardizes a way to obtain the name, version, and attributes of a Flipper device. */
struct LF_PACKED _lf_configuration {
	/* The human readable name of the device. */
	char name[16];
	/* An identifier unique to the device. */
	lf_crc_t identifier;
	/* The device's firmware version. */
	lf_version_t version;
	/* The attributes of the device. 3 (attach by default), 2:1 (word length), 0 (endianness) */
	uint8_t attributes;
};

/* Describes a device capible of responding to FMR packets. */
struct _lf_device {
	struct _lf_configuration configuration;
	/* A pointer to the endpoint through which packets will be transferred. */
	struct _lf_endpoint *endpoint;
	/* The current error state of the device. */
	lf_error_t error;
};

/* ~ Declare the virtual interface for this driver. ~ */
extern const struct _flipper {
	/* Attaches the current instance of libflipper to the first available device over the default endpoint. */
	int (* const attach)(void);
	/* Selects a previously attached Flipper device and routes all calls to it. */
	int (* const select)(struct _lf_device *device);
	/* Disconnects a previously attached Flipper device from libflipper. */
	int (* const detach)(struct _lf_device *device);
	/* Safely destroys all libflipper state before termination. */
	int (* const exit)(void);
} flipper;

typedef struct _lf_ll *lf_event_list;
extern lf_event_list lf_registered_events;
#define lf_get_event_list() lf_registered_events

typedef struct _lf_ll *lf_device_list;
extern lf_device_list lf_attached_devices;
#define lf_get_device_list() lf_attached_devices

extern struct _lf_device *lf_current_device;
inline void lf_set_current_device(struct _lf_device *device) {
	lf_current_device = device;
}
#define lf_get_current_device() lf_current_device

/* All devices must implement a self referential interface. */
#pragma warning Remove this.
struct _lf_device lf_self;

/* Standardizes the notion of a module. */
struct _lf_module {
	/* A string containing the module's name. */
	char *name;
	/* A string giving the description of a module. */
	char *description;
	/* The version of the module. */
	lf_version_t version;
	/* The module's identifier. */
	lf_crc_t identifier;
	/* The module's index. */
	uint16_t index;
	/* The pointer to a pointer to the device upon which the module's counterpart is located. */
	struct _lf_device **device;
};

/* Macro for easily generating module structures. */
#define LF_MODULE(symbol, name, description) \
	struct _lf_module symbol = { \
		name, \
		description, \
		LF_VERSION, \
		0, \
		0, \
		&lf_get_current_device() \
	};

#ifdef PLATFORM_HEADER
/* Include platform specific declarations. */
#include PLATFORM_HEADER
/* NOTE: The PLATFORM_HEADER macro is passed as a preprocessor flag during compilation. */
#endif

struct _lf_device *lf_device_create(struct _lf_endpoint *endpoint);

/* Attaches to a device. */
int lf_attach(struct _lf_device *device);
int lf_detach(struct _lf_device *device);
int lf_register_endpoint(struct _lf_endpoint *endpoint);
void lf_finish(void);

/* -------------- OLD API -------------- */

/* ~ Declare the prototypes for all functions exposed by this driver. ~ */
int flipper_attach(void);
int flipper_select(struct _lf_device *device);
int flipper_detach(struct _lf_device *device);
int flipper_exit(void);

fmr_return lf_invoke(struct _lf_module *module, fmr_function function, struct _fmr_parameters *parameters);

/* Moves data from the address space of the host to that of the device. */
int lf_push(struct _lf_module *module, fmr_function function, void *source, lf_size_t length, struct _fmr_parameters *parameters);
/* Moves data from the address space of the device to that of the host. */
int lf_pull(struct _lf_module *module, fmr_function function, void *destination, lf_size_t length, struct _fmr_parameters *parameters);

/* Load the device's configuration information. */
int lf_load_configuration(struct _lf_device *device);
/* Provides a checksum for a given block of data. */
lf_crc_t lf_crc(void *source, lf_size_t length);

/* Obtains a result from a device. */
int lf_get_result(struct _lf_device *device, struct _fmr_result *result);
/* Sends a packet to the specified device. */
int lf_transfer(struct _lf_device *device, struct _fmr_packet *packet);
/* Retrieves a packet from the specified device. */
int lf_retrieve(struct _lf_device *device, struct _fmr_result *response);
/* Binds a module structure to its device counterpart. */
int lf_bind(struct _lf_module *module);

/* Experimental: Load an application into RAM and execute it. */
int lf_ram_load(struct _lf_device *device, void *source, lf_size_t length);

/* Prints verbose information about the packet disassembly. */
void lf_debug_packet(struct _fmr_packet *packet, size_t length);
void lf_debug_result(struct _fmr_result *result);

#endif
