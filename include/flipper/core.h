/* core.h - Exposes all types and macros provided by the Flipper Toolbox. */

#ifndef __lf_core_h__
#define __lf_core_h__

/* The current version of libflipper. */
#define LF_VERSION 0x0001

/* Include the standard library headers. */
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Identification information for the USB controller. */
#define USB_VENDOR_ID	0x16C0
#define USB_PRODUCT_ID	0x0480
#define USB_USAGE_PAGE	0xFFAB
#define USB_USAGE		0x0200

/* NOTE: Summing the size parameters of each endpoints below should be less than or equal to 160. */

#define USB_IN_MASK            0x80
#define INTERRUPT_IN_ENDPOINT  (0x01 | USB_IN_MASK)
#define INTERRUPT_IN_SIZE      32
#define INTERRUPT_OUT_ENDPOINT 0x02
#define INTERRUPT_OUT_SIZE     64
#define BULK_IN_ENDPOINT       (0x03 | USB_IN_MASK)
#define BULK_IN_SIZE           32
#define BULK_OUT_ENDPOINT      0x04
#define BULK_OUT_SIZE          32

/* Terminal colors. */
#define KNRM  "\x1B[0m"
#define KRED  "\x1B[31m"
#define KBLU  "\x1B[34m"
#define KYEL  "\x1B[33m"

/* Used to contain the result of checksumming operations. */
typedef uint16_t lf_id_t;
/* Describes a type used to contain libflipper error codes. */
typedef uint32_t lf_error_t;
/* Describes a pointer to an address within non-volatile memory. */
typedef uint32_t nvm_p;

/* Used to quantify block sizes sent accross different platforms. */
typedef uint32_t lf_size_t;
#define LF_SIZE_T_MAX UINT32_MAX

/* Used to quantify the version of modules in a standardized format. */
typedef uint16_t lf_version_t;

#define lf_success 0
#define lf_error -1

/* Computes the greatest integer from the result of the division of x by y. */
#define lf_ceiling(x, y) ((x + y - 1) / y)

/* Macros that quantify device attributes. */
#define lf_device_8bit (fmr_int8_t << 1)
#define lf_device_16bit (fmr_int16_t << 1)
#define lf_device_32bit (fmr_int32_t << 1)
#define lf_device_big_endian 1
#define lf_device_little_endian 0

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
#define little(x)	((((uint16_t)(x)) << 8 ) | (((uint16_t)(x)) >> 8))
#define little32(x) ((((uint32_t)(x)) << 16 ) | (((uint32_t)(x)) >> 16))

/* Standardizes a way to obtain the name, version, and attributes of a Flipper device. */
struct __attribute__((__packed__)) _lf_configuration {
	/* The human readable name of the device. */
	char name[16];
	/* An identifier unique to the device. */
	lf_id_t identifier;
	/* The device's firmware version. */
	lf_version_t version;
	/* The attributes of the device. 3 (attach by default), 2:1 (word length), 0 (endianness) */
	uint8_t attributes;
};

/* Standardizes interaction with a physical hardware bus for the transmission of arbitrary data. */
struct _lf_endpoint {
	/* Configures the endpoint hardware given an arbitrary parameter. */
	int (* configure)();
	/* Indicates whether or not the hardware is ready to send or receive data. */
	uint8_t (* ready)(void);
	/* Sends a single byte through the endpoint. */
	void (* put)(uint8_t byte);
	/* Retrieves a single byte from the endpoint. */
	uint8_t (* get)(void);
	/* Transmits a block of data through the endpoint. */
	int (* push)(void *source, lf_size_t length);
	/* Receives a block of data from the endpoint. */
	int (* pull)(void *destination, lf_size_t length);
	/* Destroys any state associated with the endpoint. */
	int (* destroy)();
	/* The only state associated with an endpoint; tracks endpoint specific configuration information. */
	void *record;
};

/* Describes a device capible of responding to FMR packets. */
struct _lf_device {
	struct _lf_configuration configuration;
	/* A pointer to the endpoint through which packets will be transferred. */
	const struct _lf_endpoint *endpoint;
	/* The current error state of the device. */
	lf_error_t error;
	/* Describes whether or not errors generated on this device will produce side effects on the host. */
	uint8_t errors_generate_side_effects;
	/* The next device in the list of attached devices. */
	struct _lf_device *next;
};

/* Provides a checksum for a given block of data. */
lf_id_t lf_checksum(void *source, size_t length);

/* Include all message runtime related declarations. */
#include <flipper/fmr.h>

#endif
