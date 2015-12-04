#ifndef __pwm_h__

#define __pwm_h__

#include <flipper/types.h>

#include <platform/at91sam.h>

extern const struct _pwm {

	void (* configure_clockA)(uint8_t preA, uint8_t divA);

	void (* configure_clockB)(uint8_t preB, uint8_t divB);

} pwm_controller;

extern const struct _pwm_ch {

	void (* configure)(void);

	void (* set_clock)(uint8_t clock);

	void (* set_left_aligned)(void);

	void (* set_center_aligned)(void);

	void (* set_polarity)(bool);

	void (* enable)(void);

	void (* disable)(void);

	void (* enable_interrupt)(void);

	void (* disable_interrupt)(void);

	bool (* enabled)(void);

	bool (* interrupt_enabled)(void);

	bool (* finished_cycle)(void);

} pwm0, pwm1, pwm2, pwm3;

#ifdef __private_include__

/* ~--------------------------------- PWMC ---------------------------------~ */

enum { _pwm_configure_clockA, _pwm_configure_clockB };

extern void pwmc_configure_clockA(uint8_t preA, uint8_t divA);

extern void pwmc_configure_clockB(uint8_t preB, uint8_t divB);

/* ~--------------------------------- PWM0 ---------------------------------~ */

enum { _pwm0_configure, _pwm0_set_clock, _pwm0_set_left_aligned, _pwm0_set_polarity, _pwm0_set_enabled, _pwm0_set_interrupt, _pwm0_enabled, _pwm0_interrupt_enabled, _pwm0_finished_cycle };

extern void pwm0_configure(void);

extern void pwm0_set_clock(uint8_t clock);

extern void pwm0_set_left_aligned(void);

extern void pwm0_set_center_aligned(void);

extern void pwm0_set_polarity(bool);

extern void pwm0_enable(void);

extern void pwm0_disable(void);

extern void pwm0_enable_interrupt(void);

extern void pwm0_disable_interrupt(void);

extern bool pwm0_enabled(void);

extern bool pwm0_interrupt_enabled(void);

extern bool pwm0_finished_cycle(void);

/* ~--------------------------------- PWM1 ---------------------------------~ */

enum { _pwm1_configure, _pwm1_set_clock, _pwm1_set_left_aligned, _pwm1_set_polarity, _pwm1_set_enabled, _pwm1_set_interrupt, _pwm1_enabled, _pwm1_interrupt_enabled, _pwm1_finished_cycle };

extern void pwm1_configure(void);

extern void pwm1_set_clock(uint8_t clock);

extern void pwm1_set_left_aligned(void);

extern void pwm1_set_center_aligned(void);

extern void pwm1_set_polarity(bool);

extern void pwm1_enable(void);

extern void pwm1_disable(void);

extern void pwm1_enable_interrupt(void);

extern void pwm1_disable_interrupt(void);

extern bool pwm1_enabled(void);

extern bool pwm1_interrupt_enabled(void);

extern bool pwm1_finished_cycle(void);

/* ~--------------------------------- PWM2 ---------------------------------~ */

enum { _pwm2_configure, _pwm2_set_clock, _pwm2_set_left_aligned, _pwm2_set_polarity, _pwm2_set_enable, _pwm2_set_interrupt, _pwm2_enabled, _pwm2_interrupt_enabled, _pwm2_finished_cycle };

extern void pwm2_configure(void);

extern void pwm2_set_clock(uint8_t clock);

extern void pwm2_set_left_aligned(void);

extern void pwm2_set_center_aligned(void);

extern void pwm2_set_polarity(bool);

extern void pwm2_enable(void);

extern void pwm2_disable(void);

extern void pwm2_enable_interrupt(void);

extern void pwm2_disable_interrupt(void);

extern bool pwm2_enabled(void);

extern bool pwm2_interrupt_enabled(void);

extern bool pwm2_finished_cycle(void);

/* ~--------------------------------- PWM3 ---------------------------------~ */

enum { _pwm3_configure, _pwm3_set_clock, _pwm3_set_left_aligned, _pwm3_set_polarity, _pwm3_set_enabled, _pwm3_set_interrupt, _pwm3_enabled, _pwm3_interrupt_enabled, _pwm3_finished_cycle };

extern void pwm3_configure(void);

extern void pwm3_set_clock(uint8_t clock);

extern void pwm3_set_left_aligned(void);

extern void pwm3_set_center_aligned(void);

extern void pwm3_set_polarity(bool);

extern void pwm3_enable(void);

extern void pwm3_disable(void);

extern void pwm3_enable_interrupt(void);

extern void pwm3_disable_interrupt(void);

extern bool pwm3_enabled(void);

extern bool pwm3_interrupt_enabled(void);

extern bool pwm3_finished_cycle(void);

#endif // #ifdef __private_include__

#endif // #ifndef __pwm_h__
