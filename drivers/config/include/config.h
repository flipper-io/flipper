#ifndef __config_h__
#define __config_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

typedef enum { CONFIG_NAME } lf_config;

typedef uint16_t uintconfig_t;

/* ~ Declare the virtual driver object. ~ */
extern const struct _config {

	void (* configure)(void);
	void (* write)(lf_config key, uintconfig_t value);
	uintconfig_t (* read)(lf_config key);

} config;

#ifdef __private_include__

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _config_configure, _config_write, _config_read };

/* ~ Declare all function prototypes for this driver. ~ */
void config_configure(void);
void config_write(lf_config key, uintconfig_t value);
uintconfig_t config_read(lf_config key);

#endif
#endif
