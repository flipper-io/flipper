#define __private_include__

#include <pwm/pwm.h>

/* ~------------------ PWM Controller ------------------~ */

const struct _pwm pwm_contoller = {

	pwmc_configure_clockA,

	pwmc_configure_clockB

};

/* ~----------------------- PWM0 -----------------------~ */

const struct _pwm_ch pwm0 = {

	pwm0_configure,

	pwm0_set_clock,

	pwm0_set_left_aligned,

	pwm0_set_center_aligned,

	pwm0_set_polarity,

	pwm0_enable,

	pwm0_disable,

	pwm0_enable_interrupt,

	pwm0_disable_interrupt,

	pwm0_enabled,

	pwm0_interrupt_enabled,

	pwm0_finished_cycle

};

/* ~----------------------- PWM1 -----------------------~ */

const struct _pwm_ch pwm1 = {

	pwm1_configure,

	pwm1_set_clock,

	pwm1_set_left_aligned,

	pwm1_set_center_aligned,

	pwm1_set_polarity,

	pwm1_enable,

	pwm1_disable,

	pwm1_enable_interrupt,

	pwm1_disable_interrupt,

	pwm1_enabled,

	pwm1_interrupt_enabled,

	pwm1_finished_cycle

};

/* ~----------------------- PWM2 -----------------------~ */

const struct _pwm_ch pwm2 = {

	pwm2_configure,

	pwm2_set_clock,

	pwm2_set_left_aligned,

	pwm2_set_center_aligned,

	pwm2_set_polarity,

	pwm2_enable,

	pwm2_disable,

	pwm2_enable_interrupt,

	pwm2_disable_interrupt,

	pwm2_enabled,

	pwm2_interrupt_enabled,

	pwm2_finished_cycle

};

/* ~----------------------- PWM3 -----------------------~ */

const struct _pwm_ch pwm3 = {

	pwm3_configure,

	pwm3_set_clock,

	pwm3_set_left_aligned,

	pwm3_set_center_aligned,

	pwm3_set_polarity,

	pwm3_enable,

	pwm3_disable,

	pwm3_enable_interrupt,

	pwm3_disable_interrupt,

	pwm3_enabled,

	pwm3_interrupt_enabled,

	pwm3_finished_cycle

};
