#define __private_include__
#include <flipper/flipper.h>
#include <flipper/modules.h>

/* ~ Provide the definition for this standard module. ~ */
struct _fmr_module _cpu = {
    "cpu",
    "Provides control over CPU power, reset, DFU, etc.",
    LF_VERSION,
    _cpu_id,
    NULL
};

void cpu_configure(void) {
    lf_invoke(&_cpu, _cpu_configure, NULL);
}

void cpu_reset(void) {
    lf_invoke(&_cpu, _cpu_reset, NULL);
}

void cpu_halt(void) {
    lf_invoke(&_cpu, _cpu_halt, NULL);
}

void cpu_power(uint8_t power) {
    lf_invoke(&_cpu, _cpu_power, fmr_args(fmr_int8(power)));
}

void cpu_dfu(void) {
    lf_invoke(&_cpu, _cpu_dfu, NULL);
}
