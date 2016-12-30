#ifndef __fld_h__
#define __fld_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>
#include <flipper/fmr.h>

#ifdef __private_include__

/* Declare the virtual interface for this module. */
extern const struct _fld {
	int (* configure)(void);
	int (* bind)(lf_crc_t identifier);
#ifdef __fld_upload_symbols__
	/* Uploads a module from the host's filesystem to the device. */
	int (* load)(char *path, void **interface);
#endif
} fld;

/* Declare the FMR overlay for this driver. */
enum { _fld_configure, _fld_bind };

/* The fmr_module structure for this module. */
extern struct _lf_module _fld;

/* Declare each prototype for all functions within this driver. */
int fld_configure(void);
/* Loads a module instance on the device and returns its module index. */
int fld_bind(lf_crc_t identifier);

#ifdef __fld_upload_symbols__
int fld_load(char *path, void **interface);
#endif

#endif
#endif
