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
#include <flipper/device.h>
#include <flipper/module.h>
#include <flipper/fmr.h>
#include <flipper/ll.h>
#include <flipper/dyld.h>

typedef struct _lf_ll *lf_device_list;
extern lf_device_list lf_attached_devices;

extern struct _lf_device *lf_current_device;
void lf_set_current_device(struct _lf_device *device);
struct _lf_device *lf_get_current_device(void);

/* Attaches to a device. */
int lf_attach(struct _lf_device *device);
int lf_detach(struct _lf_device *device);
int lf_select(struct _lf_device *device);

/* Performs a remote procedure call to a module's function. */
lf_return_t lf_invoke(struct _lf_device *device, char *module, lf_function function, lf_type ret, struct _lf_ll *args);
/* Moves data from the address space of the host to that of the device. */
int lf_push(struct _lf_device *device, void *dst, void *src, size_t len);
/* Moves data from the address space of the device to that of the host. */
int lf_pull(struct _lf_device *device, void *dst, void *src, size_t len);
/* Gets the module index. */
int lf_dyld(struct _lf_device *device, char *module);

/* Closes the library. */
int lf_exit(void);

/* Provides a checksum for a given block of data. */
lf_crc_t lf_crc(const void *src, uint32_t length);

/* Obtains a result from a device. */
int lf_get_result(struct _lf_device *device, struct _fmr_result *result);
/* Sends a packet to the specified device. */
int lf_transfer(struct _lf_device *device, struct _fmr_packet *packet);
/* Retrieves a packet from the specified device. */
int lf_retrieve(struct _lf_device *device, struct _fmr_result *response);

/* Prints verbose information about the packet disassembly. */
void lf_debug_packet(struct _fmr_packet *packet, uint32_t length);
void lf_debug_result(struct _fmr_result *result);

#endif
