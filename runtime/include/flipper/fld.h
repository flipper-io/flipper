#ifndef __fld_h__
#define __fld_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper.h>

/* Declare the prototypes for all of the functions within this module. */
int fld_configure(void);
/* Returns the index of a loaded module. */
int fld_index(lf_crc_t identifier);

#endif
