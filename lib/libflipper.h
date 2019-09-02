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

#include "defines.h"
#include "device.h"
#include "dyld.h"
#include "error.h"
#include "fmr.h"
#include "ll.h"
#include "module.h"

/* Returns the short version string of the library. */
const char *lf_get_git_hash(void);

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
int lf_invoke(struct _lf_device *device, const char *module, lf_function function, lf_type ret, lf_return_t *retval,
              struct _lf_ll *args);

/* Moves data from the address space of the host to that of the device. */
int lf_push(struct _lf_device *device, void *dst, void *src, uint32_t len);

/* Moves data from the address space of the device to that of the host. */
int lf_pull(struct _lf_device *device, void *dst, void *src, uint32_t len);

/* Gets the module index. */
int lf_dyld(struct _lf_device *device, const char *module, uint16_t *idx);

/* Allocates memory on the device. */
int lf_malloc(struct _lf_device *device, uint32_t size, void **ptr);

/* Frees memory on the device. */
int lf_free(struct _lf_device *device, void *ptr);

/* Provides a checksum for a given block of data. */
int lf_crc(const void *src, uint32_t length, lf_crc_t *crc);

/* ---------- DEBUG ---------- */

enum { LF_DEBUG_LEVEL_OFF, LF_DEBUG_LEVEL_WARNINGS, LF_DEBUG_LEVEL_ERRORS, LF_DEBUG_LEVEL_ALL };

/* Sets library debug verbosity. */
void lf_set_debug_level(int level);

/* Prints verbose information about the packet disassembly. */
void lf_debug_packet(struct _fmr_packet *packet);

/* Prints verbose information about a function call. */
void lf_debug_result(struct _fmr_result *result);

#endif
