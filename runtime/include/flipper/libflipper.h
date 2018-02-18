/*
 * libflipper.h
 *
 * Core API specifications for interaction with Flipper hardware.
 *
 */

#ifndef __libflipper_h__
#define __libflipper_h__

#include <flipper/types.h>

/* The current version of libflipper. */
#define LF_VERSION 0x0001

/* If defined, imposes a timeout on USB transactions. */
#define __lf_usb_timeout__
#ifdef __lf_usb_timeout__
/* Must be between 1ms and 255ms. */
#define LF_USB_TIMEOUT_MS 200
#else
#define LF_USB_TIMEOUT_MS 0
#endif

#define LF_UART_TIMEOUT_MS 100

/* NOTE: Summing the size parameters of each endpoints below should be less than or equal to 160. */
#define USB_IN_MASK            0x80

#define INTERRUPT_IN_ENDPOINT	(0x01 | USB_IN_MASK)
#define INTERRUPT_IN_SIZE		16
#define INTERRUPT_OUT_ENDPOINT	0x02
#define INTERRUPT_OUT_SIZE		16

#define FMR_INTERFACE			0
#define BULK_IN_ENDPOINT		(0x01 | USB_IN_MASK)
#define BULK_IN_SIZE			64
#define BULK_OUT_ENDPOINT		0x02
#define BULK_OUT_SIZE			64

#define DEBUG_INTERFACE			1
#define DEBUG_IN_ENDPOINT		(0x03 | USB_IN_MASK)
#define DEBUG_IN_SIZE			32

/* The name of the default device to attach to. */
#define LF_DEFAULT_NAME "flipper"

/* If defined, uses bulk for all USB transfers. */
#define __ALL_BULK__

enum {
	LF_DEBUG_LEVEL_OFF,
	LF_DEBUG_LEVEL_WARNINGS,
	LF_DEBUG_LEVEL_ERRORS,
	LF_DEBUG_LEVEL_ALL
};

//#define __LF_DEBUG__
#ifdef __LF_DEBUG__
#define lf_debug(format, ...) printf(format"\n", ##__VA_ARGS__)
#else
#define lf_debug(format, ...)
#endif

/* Sets library debug verbosity. */
void lf_set_debug_level(int level);

/* Computes the greatest integer from the result of the division of x by y. */
#define lf_ceiling(x, y) ((x + y - 1) / y)

/* Define bit manipulation macros. */
#define lo(x) ((uint8_t)(x))
#define hi(x) ((uint8_t)(x >> 8))
#define lo16(x) ((uint16_t)(((uint32_t)(x))))
#define hi16(x) ((uint16_t)(((uint32_t)(x)) >> 16))
#define little(x) ((((uint16_t)(x)) << 8 ) | (((uint16_t)(x)) >> 8))
#define little32(x) ((((uint32_t)(x)) << 16 ) | (((uint32_t)(x)) >> 16))

#include <flipper/error.h>

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
	/* The device's selector function. Mutates modules state as appropriate for the device. */
	int (* select)(struct _lf_device *device);
	/* The device's destructor. */
	int (* release)(struct _lf_device *device);
	/* The device's context. */
	void *_ctx;
	/* The current error state of the device. */
	lf_error_t error;
};

typedef struct _lf_ll *lf_event_list;
extern lf_event_list lf_registered_events;
#define lf_get_event_list() lf_registered_events

typedef struct _lf_ll *lf_device_list;
extern lf_device_list lf_attached_devices;

extern struct _lf_device *lf_current_device;
void lf_set_current_device(struct _lf_device *device);
struct _lf_device *lf_get_current_device(void);

/* Standardizes the notion of a module. */
struct _lf_module {
	/*! A string containing the module's name. */
	const char *name;
	/*! A string giving the description of a module. */
	const char *description;
	/*! The version of the module. */
	lf_version_t version;
	/*! The module's identifier. */
	lf_crc_t identifier;
	/*! The module's loaded index. */
	int index;
	/*! The module's binary data. */
	void *data;
	/*! The binary data size. */
	uint32_t *psize;
};

/* Macro for easily generating module structures. */
#define LF_MODULE(symbol, name, description, pdata, plen) \
	struct _lf_module symbol = { \
		name, \
		description, \
		LF_VERSION, \
		0, \
		-1, \
		pdata, \
		plen \
	};

struct _lf_device *lf_device_create(struct _lf_endpoint *endpoint, int (* select)(struct _lf_device *device), int (* release)(struct _lf_device *device), size_t context_size);
int lf_device_release(struct _lf_device *device);

/* Attaches to a device. */
int lf_attach(struct _lf_device *device);
int lf_detach(struct _lf_device *device);
int lf_select(struct _lf_device *device);

#include <flipper/fmr.h>
#include <flipper/endpoint.h>
#include <flipper/ll.h>

/* Performs a remote procedure call to a module's function. */
lf_return_t lf_invoke(struct _lf_device *device, struct _lf_module *module, lf_function function, lf_type ret, struct _lf_ll *args);
/* Moves data from the address space of the host to that of the device. */
lf_return_t lf_push(struct _lf_device *device, struct _lf_module *module, lf_function function, void *source, lf_size_t length, struct _lf_ll *args);
/* Moves data from the address space of the device to that of the host. */
lf_return_t lf_pull(struct _lf_device *device, struct _lf_module *module, lf_function function, void *destination, lf_size_t length, struct _lf_ll *args);

/* Closes the library. */
int lf_exit(void);

/* Load the device's configuration information. */
int lf_load_configuration(struct _lf_device *device);
/* Provides a checksum for a given block of data. */
lf_crc_t lf_crc(const void *source, size_t length);

/* Obtains a result from a device. */
int lf_get_result(struct _lf_device *device, struct _fmr_result *result);
/* Sends a packet to the specified device. */
int lf_transfer(struct _lf_device *device, struct _fmr_packet *packet);
/* Retrieves a packet from the specified device. */
int lf_retrieve(struct _lf_device *device, struct _fmr_result *response);
/* Binds a module structure to its device counterpart. */
int lf_bind(struct _lf_device *device, struct _lf_module *module);

/* Experimental: Load an application into RAM and execute it. */
int lf_load(struct _lf_device *device, void *source, lf_size_t length);

/* Prints verbose information about the packet disassembly. */
void lf_debug_packet(struct _fmr_packet *packet, size_t length);
void lf_debug_result(struct _fmr_result *result);

#endif
