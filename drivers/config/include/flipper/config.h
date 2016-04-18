#ifndef __config_h__
#define __config_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

enum { CONFIG_NAME };

/* ~ Declare the virtual driver object. ~ */
extern const struct _config {

	void (* configure)(void);
	void (* write)(uint8_t key, uint16_t value);
	uint16_t (* read)(uint8_t key);

} config;

#ifdef __private_include__

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _config_configure, _config_write, _config_read };

/* ~ Declare all function prototypes for this driver. ~ */
void config_configure(void);
void config_write(uint8_t key, uint16_t value);
uint16_t config_read(uint8_t key);

#endif
#endif
