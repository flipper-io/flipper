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

/* Identification information for the USB controller. */
#define USB_VENDOR_ID	0x16C0
#define USB_PRODUCT_ID	0x0480
#define USB_USAGE_PAGE	0xFFAB
#define USB_USAGE		0x0200

/* The name of the default device to attach to. */
#define LF_DEFAULT_NAME "flipper"

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

/* Include all types needed by libflipper. */
#include <flipper/types.h>
/* Include the error primitives needed by libflipper. */
#include <flipper/error.h>
/* Include the message runtime primitives needed by libflipper. */
#include <flipper/fmr.h>

/* Macros that quantify device attributes. */
#define lf_device_8bit (fmr_int8_t << 1)
#define lf_device_16bit (fmr_int16_t << 1)
#define lf_device_32bit (fmr_int32_t << 1)
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

/* Standardizes interaction with a physical hardware bus for the transmission of arbitrary data. */
struct _lf_endpoint {
	/* Configures the endpoint record given an arbitrary set of parameters. */
	int (* configure)();
	/* Indicates whether or not the endpoint is ready to send or receive data. */
	uint8_t (* ready)(struct _lf_endpoint *this);
	/* Sends a single byte through the endpoint. */
	void (* put)(struct _lf_endpoint *this, uint8_t byte);
	/* Retrieves a single byte from the endpoint. */
	uint8_t (* get)(struct _lf_endpoint *this);
	/* Transmits a block of data through the endpoint. */
	int (* push)(struct _lf_endpoint *this, void *source, lf_size_t length);
	/* Receives a block of data from the endpoint. */
	int (* pull)(struct _lf_endpoint *this, void *destination, lf_size_t length);
	/* Destroys any state associated with the endpoint. */
	int (* destroy)();
	/* The only state associated with an endpoint; tracks endpoint specific configuration information. */
	void *record;
};

/* Describes a device capible of responding to FMR packets. */
struct _lf_device {
	struct _lf_configuration configuration;
	/* A pointer to the endpoint through which packets will be transferred. */
	struct _lf_endpoint *endpoint;
	/* The current error state of the device. */
	lf_error_t error;
};

/* All devices must implement a self referential interface. */
extern struct _lf_device lf_self;

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
	uint8_t index;
	/* The pointer to a pointer to the device upon which the module's counterpart is located. */
	struct _lf_device **device;
};

/* Macro for easily generating module structures. */
#define LF_MODULE(symbol, name, description, index) \
	struct _lf_module symbol = { \
		name, \
		description, \
		LF_VERSION, \
		0, \
		index, \
		&flipper.device \
	};

#ifdef PLATFORM_HEADER
/* Include platform specific declarations. */
#include PLATFORM_HEADER
/* NOTE: The PLATFORM_HEADER macro is passed as a preprocessor flag during compilation. */
#endif

/* ~ Declare the virtual interface for this driver. ~ */
extern struct _flipper {
	/* Attaches the current instance of libflipper to the first available device over the default endpoint. */
	struct _lf_device *(* attach)(void);
	/* Attaches to a Flipper device by name over the USB endpoint. */
	struct _lf_device *(* attach_usb)(char *name);
	/* Attaches to a Flipper device by name and hostname/IP over the network endpoint. */
	struct _lf_device *(* attach_network)(char *name, char *hostname);
	/* Attaches to a Flipper device by name over an arbitrary endpoint. */
	struct _lf_device *(* attach_endpoint)(char *name, struct _lf_endpoint *endpoint);
	/* Selects a previously attached Flipper device and routes all calls to it. */
	int (* select)(struct _lf_device *device);
	/* Disconnects a previously attached Flipper device from libflipper. */
	int (* detach)(struct _lf_device *device);
	/* Safely destroys all libflipper state before termination. */
	int (* exit)(void);
	/* Stores the last observed error code. */
	lf_error_t error_code;
	/* Global flag that indicates whether or not error_raise() should print to stderr and call exit(). */
	uint8_t errors_cause_side_effects;
	/* Points to the actively selected device with which interaction will take place. */
	struct _lf_device *device;
} flipper;

#ifdef __private_include__

/* NOTE: Definitely move this. */
extern struct _lf_endpoint lf_bridge_ep;
/* ~ Declare the prototypes for all functions exposed by this driver. ~ */
int lf_bridge_configure();
uint8_t lf_bridge_ready(struct _lf_endpoint *this);
void lf_bridge_put(struct _lf_endpoint *this, uint8_t byte);
uint8_t lf_bridge_get(struct _lf_endpoint *this);
int lf_bridge_push(struct _lf_endpoint *this, void *source, lf_size_t length);
int lf_bridge_pull(struct _lf_endpoint *this, void *destination, lf_size_t length);
int lf_bridge_destroy(struct _lf_endpoint *this);

/* ~ Declare the prototypes for all functions exposed by this driver. ~ */
extern struct _lf_device *flipper_attach(void);
extern struct _lf_device *flipper_attach_usb(char *name);
extern struct _lf_device *flipper_attach_network(char *name, char *hostname);
extern struct _lf_device *flipper_attach_endpoint(char *name, struct _lf_endpoint *endpoint);
extern int flipper_select(struct _lf_device *device);
extern int flipper_detach(struct _lf_device *device);
extern int flipper_exit(void);

/**
 * @brief Invokes a function in a module with a list of parameters.
 *
 * @param module The module in which the function resides.
 * @param function The function to be invoked.
 * @param paramters The list of parameters to be passed to the function.
 *
 */
extern fmr_return lf_invoke(struct _lf_module *module, fmr_function function, struct _fmr_list *parameters);
/* Moves data from the address space of the host to that of the device. */
extern int lf_push(struct _lf_module *module, fmr_function function, void *source, lf_size_t length, struct _fmr_list *parameters);
/* Moves data from the address space of the device to that of the host. */
extern int lf_pull(struct _lf_module *module, fmr_function function, void *destination, lf_size_t length, struct _fmr_list *parameters);

/* Load the device's configuration information. */
extern int lf_load_configuration(struct _lf_device *device);
/* Provides a checksum for a given block of data. */
extern lf_crc_t lf_crc(void *source, lf_size_t length);

/* Obtains a result from a device. */
extern int lf_get_result(struct _lf_device *device, struct _fmr_result *result);
/* Sends a packet to the specified device. */
extern int lf_transfer(struct _lf_device *device, struct _fmr_packet *packet);
/* Retrieves a packet from the specified device. */
extern int lf_retrieve(struct _lf_device *device, struct _fmr_result *response);

/* Prints verbose information about the packet disassembly. */
extern void lf_debug_packet(struct _fmr_packet *packet, size_t length);

#endif
#endif
