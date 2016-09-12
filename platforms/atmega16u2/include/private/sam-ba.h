#ifndef __sam_ba_h__
#define __sam_ba_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

#ifdef __private_include__

/* Configures the hardare used to control the primary CPU. */
void sam_configure(void);
/* Holds the primary CPU in reset. */
void sam_suspend(void);
/* Take the primary CPU out of reset. */
void sam_engage(void);
/* Powers the primary CPU on or off. */
void sam_power(uint8_t power);
/* Resets the primary CPU. */
void sam_reset(void);
/* Places the primary CPU into device firmware update mode. */
void sam_load_dfu(void);
/* Erases the internal flash memory of the primary CPU and puts it into factory reset. */
void sam_erase(void);

#endif
#endif
