#define __private_include__
#include <flipper/flipper.h>
#include <flipper/carbon/cpu.h>

int cpu_configure(void) {
	return lf_invoke(&_cpu, _cpu_configure, NULL);
}

void cpu_reset(void) {
	lf_invoke(&_cpu, _cpu_reset, NULL);
}

void cpu_cycle(void) {
	lf_invoke(&_cpu, _cpu_cycle, NULL);
}

void cpu_halt(void) {
	lf_invoke(&_cpu, _cpu_halt, NULL);
}

void cpu_power(uint8_t power) {
	lf_invoke(&_cpu, _cpu_power, fmr_args(fmr_infer(power)));
}

lf_error_t cpu_dfu(void) {
	return lf_invoke(&_cpu, _cpu_dfu, NULL);
}
