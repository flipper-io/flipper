#define __private_include__
#include <flipper/flipper.h>
#include <flipper/modules.h>

/* ~ Provide the definition for this standard module. ~ */
LF_MODULE(_cpu, "cpu", "Provides control over the CPU of the device.", _cpu_id);

int cpu_configure(void) {
	return lf_invoke(&_cpu, _cpu_configure, NULL);
}

void cpu_reset(void) {
	lf_invoke(&_cpu, _cpu_reset, NULL);
}

void cpu_halt(void) {
	lf_invoke(&_cpu, _cpu_halt, NULL);
}

void cpu_power(uint8_t power) {
	lf_invoke(&_cpu, _cpu_power, fmr_args(fmr_infer(power)));
}

void cpu_dfu(void) {
	lf_invoke(&_cpu, _cpu_dfu, NULL);
}
