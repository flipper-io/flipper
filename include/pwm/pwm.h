#ifndef __pwm_h__

#define __pwm_h__

#include <flipper/types.h>

#include <platform/at91sam.h>

extern const struct _pwm {

	void (* configure)(void);

	void (* enable)(void);

	void (* disable)(void);

	void (* enable_interrupt)(void);

	void (* disable_interrupt)(void);

	bool (* pwm_enabled)(void);

	bool (* pwm_interrupt_enabled)(void);

	bool (* pwm_finished_cycle)(void);

} pwm0, pwm1, pwm2, pwm3;

#ifdef __private_include__

/* ~ ~------------------------------- PWM0 -------------------------------~ ~ */

enum { _pwm0_configure, _pwm0_set_enabled, _pwm0_set_interrupt, _pwm0_enabled, _pwm0_interrupt_enabled, _pwm0_finished_cycle };

extern void pwm0_configure(void);

extern void pwm0_enable(void);

extern void pwm0_disable(void);

extern void pwm0_enable_interrupt(void);

extern void pwm0_disable_interrupt(void);

extern bool pwm0_enabled(void);

extern bool pwm0_interrupt_enabled(void);

extern bool pwm0_finished_cycle(void);

/* ~ ~------------------------------- PWM1 -------------------------------~ ~ */

enum { _pwm1_configure, _pwm1_set_enabled, _pwm1_set_interrupt, _pwm1_enabled, _pwm1_interrupt_enabled, _pwm1_finished_cycle };

extern void pwm1_configure(void);

extern void pwm1_enable(void);

extern void pwm1_disable(void);

extern void pwm1_enable_interrupt(void);

extern void pwm1_disable_interrupt(void);

extern bool pwm1_enabled(void);

extern bool pwm1_interrupt_enabled(void);

extern bool pwm1_finished_cycle(void);

/* ~ ~------------------------------- PWM2 -------------------------------~ ~ */

enum { _pwm2_configure, _pwm2_set_enable, _pwm2_set_interrupt, _pwm2_enabled, _pwm2_interrupt_enabled, _pwm2_finished_cycle };

extern void pwm2_configure(void);

extern void pwm2_enable(void);

extern void pwm2_disable(void);

extern void pwm2_enable_interrupt(void);

extern void pwm2_disable_interrupt(void);

extern bool pwm2_enabled(void);

extern bool pwm2_interrupt_enabled(void);

extern bool pwm2_finished_cycle(void);

/* ~ ~------------------------------- PWM3 -------------------------------~ ~ */

enum { _pwm3_configure, _pwm3_set_enabled, _pwm3_set_interrupt, _pwm3_enabled, _pwm3_interrupt_enabled, _pwm3_finished_cycle };

extern void pwm3_configure(void);

extern void pwm3_enable(void);

extern void pwm3_disable(void);

extern void pwm3_enable_interrupt(void);

extern void pwm3_disable_interrupt(void);

extern bool pwm3_enabled(void);

extern bool pwm3_interrupt_enabled(void);

extern bool pwm3_finished_cycle(void);

#endif // #ifdef __private_include__

#endif // #ifndef __pwm_h__
