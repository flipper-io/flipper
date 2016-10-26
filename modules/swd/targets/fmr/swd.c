#define __private_include__
#include <flipper/flipper.h>
#include <flipper/modules.h>

/* ~ Provide the definition for this standard module. ~ */
LF_MODULE(_swd, "swd", "Interfaces with the device's single wire debug unit.", _swd_id);

int swd_configure(void) {
	return lf_invoke(&_swd, _swd_configure, NULL);
}
