#define __private_include__
#include <flipper/carbon/modules/cpu.h>

#ifdef __use_cpu__
/* Define the virtual interface for this module. */
const struct _cpu cpu = {
	cpu_configure,
	cpu_reset,
	cpu_cycle,
	cpu_halt,
	cpu_power,
	cpu_dfu
};
#endif
