/*
 * libflipper.h
 *
 * Core API specifications for interaction with Flipper hardware.
 *
 */

#ifndef __libflipper_h__
#define __libflipper_h__

/* Include the standard library headers. */
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define FLIPPER_USB_VENDOR_ID 0x16C0
#define FLIPPER_USB_CONTROL_INTERFACE 0

/* Terminal colors. */
#define LF_CONFIG_NO_COLOR
#ifdef LF_CONFIG_NO_COLOR
#define KNRM ""
#define KGRN ""
#define KRED ""
#define KBLU ""
#define KYEL ""
#else
#define KNRM "\x1B[0m"
#define KGRN "\x1B[32m"
#define KRED "\x1B[31m"
#define KBLU "\x1B[34m"
#define KYEL "\x1B[33m"
#endif

#define lf_debug(fmt, ...) printf(fmt"\n", ##__VA_ARGS__)

/* Packed attribute. */
#define LF_PACKED __attribute__((__packed__))
/* Weak attribute. */
#define LF_WEAK __attribute__((weak))

#ifdef __clang__
#define LF_FUNC(MODULE) __attribute__((section("__TEXT,.lm." MODULE), used))
#else
#define LF_FUNC(MODULE) __attribute__((section(".lm." MODULE), used))
#endif

/* Used to contain the result of checksumming operations. */
typedef uint16_t lf_crc_t;
/* Used to quantify the version of modules in a standardized format. */
typedef uint16_t lf_version_t;
/* Used to specify return types. */
typedef uint64_t lf_return_t;

#include "error.h"
#include "device.h"
#include "module.h"
#include "fmr.h"
#include "ll.h"
#include "dyld.h"

/* ---------- STATE ---------- */

/* Attaches to a device. */
int lf_attach(struct _lf_device *device);

/* Selects an attached device. */
int lf_select(struct _lf_device *device);

/* Returns the currently selected device. */
struct _lf_device *lf_get_selected(void);

/* Detaches from an attached device. */
int lf_detach(struct _lf_device *device);

/* Releases all library state. */
int lf_exit(void);

/* ---------- CONTROL ---------- */

/* Performs a remote procedure call to a module's function. */
int lf_invoke(struct _lf_device *device, const char *module, lf_function function, lf_type ret, lf_return_t *retval, struct _lf_ll *args);

/* Moves data from the address space of the host to that of the device. */
int lf_push(struct _lf_device *device, void *dst, void *src, size_t len);

/* Moves data from the address space of the device to that of the host. */
int lf_pull(struct _lf_device *device, void *dst, void *src, size_t len);

/* Gets the module index. */
int lf_dyld(struct _lf_device *device, const char *module, int *idx);

/* Allocates memory on the device. */
int lf_malloc(struct _lf_device *device, size_t size, void **ptr);

/* Frees memory on the device. */
int lf_free(struct _lf_device *device, void *ptr);

/* Provides a checksum for a given block of data. */
int lf_crc(const void *src, uint32_t length, lf_crc_t *crc);

/* ---------- DEBUG ---------- */

enum {
	LF_DEBUG_LEVEL_OFF,
	LF_DEBUG_LEVEL_WARNINGS,
	LF_DEBUG_LEVEL_ERRORS,
	LF_DEBUG_LEVEL_ALL
};

/* Sets library debug verbosity. */
void lf_set_debug_level(int level);

/* Prints verbose information about the packet disassembly. */
void lf_debug_packet(struct _fmr_packet *packet, uint32_t length);

/* Prints verbose information about a function call. */
void lf_debug_result(struct _fmr_result *result);

#endif
