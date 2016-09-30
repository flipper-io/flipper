#ifndef __cpu_h__
#define __cpu_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

/* Declare the virtual interface for this module. */
extern const struct _cpu {
	void (* configure)(void);
	void (* reset)(void);
	void (* halt)(void);
	void (* power)(uint8_t power);
	void (* dfu)(void);
} cpu;

#ifdef __private_include__

/* Declare the FMR overlay for this driver. */
enum { _cpu_configure, _cpu_reset, _cpu_halt, _cpu_power, _cpu_dfu };

/* Declare each prototype for all functions within this driver. */
extern void cpu_configure(void);
extern void cpu_reset(void);
extern void cpu_halt(void);
extern void cpu_power(uint8_t power);
extern void cpu_dfu(void);

#endif
#endif
