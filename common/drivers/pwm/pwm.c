#define __private_include__

#include <pwm/pwm.h>

/* ~----------------------- PWM0 -----------------------~ */

const struct _pwm pwm0 = {

	pwm0_configure,

	pwm0_set_enabled,

	pwm0_set_interrupt,

	pwm0_enabled,

	pwm0_interrupt_enabled,

	pwm0_finished_cycle

};

/* ~----------------------- PWM1 -----------------------~ */

const struct _pwm pwm1 = {

	pwm1_configure,

	pwm1_set_enabled,

	pwm1_set_interrupt,

	pwm1_enabled,

	pwm1_interrupt_enabled,

	pwm1_finished_cycle

};

/* ~----------------------- PWM2 -----------------------~ */

const struct _pwm pwm2 = {

	pwm2_configure,

	pwm2_set_enabled,

	pwm2_set_interrupt,

	pwm2_enabled,

	pwm2_interrupt_enabled,

	pwm2_finished_cycle

};

/* ~----------------------- PWM3 -----------------------~ */

const struct _pwm pwm3 = {

	pwm3_configure,

	pwm3_set_enabled,

	pwm3_set_interrupt,

	pwm3_enabled,

	pwm3_interrupt_enabled,

	pwm3_finished_cycle

};
