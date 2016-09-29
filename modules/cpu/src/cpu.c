#define __private_include__
#include <flipper/cpu.h>

/* Define the virtual interface for this module. */
const struct _cpu cpu = {
	cpu_configure,
	cpu_reset,
	cpu_hault,
	cpu_power,
	cpu_dfu
};
