#ifndef __fmr_h__

#define __fmr_h__

#include <flipper/types.h>

#include <fmr/target.h>

#include <fs/crc.h>

typedef uint32_t fmr_handle;

#define fmr_bundle_id_from_string(string) checksum(string, strlen(string))

/* ~ Define the virtual interface for this driver. ~ */

extern const struct _fmr {
	
	void (* configure)(void);
	
	void (* bind)(fmr_handle *handle, uint16_t id);
	
	uint32_t (* invoke)(fmr_handle handle, uint8_t index, uint8_t argc, ...);
	
	void *(* resolve)(void *source, uint32_t length);

} fmr;

#ifdef __private_include__

/* ~ Define the overlay for this driver. ~ */

enum { _fmr_configure, _fmr_bind, _fmr_invoke, _fmr_resolve };

/* ~ Explicitly reference the functions needed by this driver. ~ */

extern void fmr_configure(void);

extern void fmr_bind(fmr_handle *handle, uint16_t id);

extern uint32_t fmr_invoke(fmr_handle handle, uint8_t index, uint8_t argc, ...);

extern void *fmr_resolve(void *source, uint32_t length);

#endif

#endif
