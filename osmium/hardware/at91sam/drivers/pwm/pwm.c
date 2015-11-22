#define __private_include__

#include <pwm/pwm.h>

void pwm_configure(AT91S_PWMC_CH *pwm) {

	/* ~ Enable PWM clock in the Power Management Controller (PMC). ~ */
	/* ~ Since this enables the entire PWM Controller, it's  not    ~ */
	/* ~ PWM channel specific.                                      ~ */

	set_bit_in_port(AT91C_ID_PWMC, AT91C_BASE_PMC -> PMC_PCER);

	/* ~ Configure clock generator. ~ */

	/* ~ Select clock for this channel. ~ */

	/* ~ Configure waveform alignment.                               ~ */
	/* ~ Set CALG (Center Aligned) to 0 for a left-aligned waveform. ~ */

	clear_bits_in_port_with_mask(pwm -> PWMC_CMR, AT91C_PWMC_CALG);

	/* ~ Configure the default period for each channel. ~ */

	/* ~ Configure the default duty cycle for each channel. ~ */

	/* ~ Configure the output waveform polarity.                 ~ */
	/* ~ The default waveform polarity is positive, set using 0. ~ */

	clear_bits_in_port_with_mask(pwm -> PWMC_CMR, AT91C_PWMC_CPOL);

}

/**
 * Sets the given PWM channel as enabled or disabled.
 * @param channel The PWM channel to enable or disable.
 * @param enabled True to enable, false to disable.
 */
void pwm_set_enabled(uint8_t channel, bool enabled) {

	/* ~ Use enable or disable register as necessary. ~ */

	if(enabled) {

		set_bits_in_port_with_mask(AT91PS_PWMC -> PWMC_ENA, channel);

	} else {

		set_bits_in_port_with_mask(AT91PS_PWMC -> PWMC_DIS, channel);

	}
}

/**
 * Sets the given PWM channel to enable or disable interrupts.
 * @param channel The PWM channel to enable or disable interrupts for.
 * @param enabled True to enable interrupts, false to disable.
 */
void pwm_set_interrupt(uint8_t channel, bool enabled) {

	/* ~ Use enable or disable register as necessary. ~ */

	if(enabled) {

		set_bits_in_port_with_mask(AT91PS_PWMC -> PWMC_IER, channel);

	} else {

		set_bits_in_port_with_mask(AT91PS_PWMC -> PWMC_IDR, channel);

	}
}

/**
 * Tells whether the given PWM channel is currently enabled or disabled.
 * @param chennel The PWM channel to check.
 * @return True if the given channel is enabled, false if it's disabled.
 */
bool pwm_enabled(uint8_t channel) {

	return get_bits_from_port_with_mask(AT91PS_PWMC -> PWMC_SR, channel);
}

/**
 * Tells whether the given PWM channel currently has interrupts enabled.
 * @param channel The PWM channel to check.
 * @return True if the given channel has interrupts enabled, false if disabled.
 */
bool pwm_interrupt_enabled(uint8_t channel) {

	return get_bits_from_port_with_mask(AT91PS_PWMC -> PWMC_IMR, channel);
}

/**
 * Tells whether a cycle has finished on the given PWM channel since the last
 * check.
 * @param channel The PWM channel to check.
 * @return True if a period cycle has completed, false otherwise.
 */
bool pwm_finished_cycle(uint8_t channel) {

	return get_bits_from_port_with_mask(AT91PS_PWMC -> PWMC_ISR, channel);
}

/* ~ ~------------------------------- PWM0 -------------------------------~ ~ */

extern void pwm0_configure(void) {

	pwm_configure(AT91C_BASE_PWMC_CH0);

}

extern void pwm0_set_enabled(bool enabled) {

	pwm_set_enabled(AT91C_PWMC_CHID0, enabled);

}

extern void pwm0_set_interrupt(bool enabled) {

	pwm_set_interrupt(AT91C_PWMC_CHID0, enabled);

}

extern bool pwm0_enabled(void) {

	return pwm_enabled(AT91C_PWMC_CHID0);

}

extern bool pwm0_interrupt_enabled(void) {

	return pwm_interrupt_enabled(AT91C_PWMC_CHID0);

}

extern bool pwm0_finished_cycle(void) {

	return pwm_finished_cycle(AT91C_PWMC_CHID0);

}

/* ~ ~------------------------------- PWM1 -------------------------------~ ~ */

extern void pwm1_configure(void) {

	pwm_configure(AT91C_BASE_PWMC_CH1);

}

extern void pwm1_set_enabled(bool enabled) {

	pwm_set_enabled(AT91C_PWMC_CHID1, enabled);

}

extern void pwm1_set_interrupt(bool enabled) {

	pwm_set_interrupt(AT91C_PWMC_CHID1, enabled);

}

extern bool pwm1_enabled(void) {

	return pwm_enabled(AT91C_PWMC_CHID1);

}

extern bool pwm1_interrupt_enabled(void) {

	return pwm_interrupt_enabled(AT91C_PWMC_CHID1);

}

extern bool pwm1_finished_cycle(void) {

	return pwm_finished_cycle(AT91C_PWMC_CHID1);

}

/* ~ ~------------------------------- PWM2 -------------------------------~ ~ */

extern void pwm2_configure(void) {

	pwm_configure(AT91C_BASE_PWMC_CH2);

}

extern void pwm2_set_enable(bool enabled) {

	pwm_set_enabled(AT91C_PWMC_CHID2, enabled);

}

extern void pwm2_set_interrupt(bool enabled) {

	pwm_set_interrupt(AT91C_PWMC_CHID2, enabled);

}

extern bool pwm2_enabled(void) {

	return pwm_enabled(AT91C_PWMC_CHID2);

}

extern bool pwm2_interrupt_enabled(void) {

	return pwm_interrupt_enabled(AT91C_PWMC_CHID2);

}

extern bool pwm2_finished_cycle(void) {

	return pwm_finished_cycle(AT91C_PWMC_CHID2);

}

/* ~ ~------------------------------- PWM3 -------------------------------~ ~ */

extern void pwm3_configure(void) {

	pwm_configure(AT91C_BASE_PWMC_CH3);

}

extern void pwm3_set_enabled(bool enabled) {

	pwm_set_enabled(AT91C_PWMC_CHID3, enabled);

}

extern void pwm3_set_interrupt(bool enabled) {

	pwm_set_interrupt(AT91C_PWMC_CHID3, enabled);

}

extern bool pwm3_enabled(void) {

	return pwm_enabled(AT91C_PWMC_CHID3);

}

extern bool pwm3_interrupt_enabled(void) {

	return pwm_interrupt_enabled(AT91C_PWMC_CHID3);

}

extern bool pwm3_finished_cycle(void) {

	return pwm_finished_cycle(AT91C_PWMC_CHID3);

}
