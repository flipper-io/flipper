#define __private_include__

#include <pwm/pwm.h>

/**
 * Powers on the PWM module on the ARM chip.
 */
void pwmc_power_on() {

	set_bit_in_port(AT91C_ID_PWMC, AT91C_BASE_PMC -> PMC_PCER);

}

/**
 * Powers off the PWM module on the ARM chip.
 */
void pwmc_power_off() {

	set_bit_in_port(AT91C_ID_PWMC, AT91C_BASE_PMC -> PMC_PCDR);

}

/**
 * Configures clock A of the PWM Controller.  Configuring the clock
 * involves defining a Prescalar value and a Divider value. The
 * available values are as follows:
 *
 *     Defining Prescalar A (preA):
 *     ====================================
 *     |   Binary   | Hex  |   Value      |
 *     | 0b00000000 | 0x00 | (MCK / 1   ) |
 *     | 0b00000001 | 0x01 | (MCK / 2   ) |
 *     | 0b00000010 | 0x02 | (MCK / 4   ) |
 *     | 0b00000011 | 0x03 | (MCK / 8   ) |
 *     | 0b00000100 | 0x04 | (MCK / 16  ) |
 *     | 0b00000101 | 0x05 | (MCK / 32  ) |
 *     | 0b00000110 | 0x06 | (MCK / 64  ) |
 *     | 0b00000111 | 0x07 | (MCK / 128 ) |
 *     | 0b00001000 | 0x08 | (MCK / 256 ) |
 *     | 0b00001001 | 0x09 | (MCK / 512 ) |
 *     | 0b00001010 | 0x0A | (MCK / 1024) |
 *     ====================================
 *
 *     Defining Divider A (divA):
 *     ===============================
 *     | Value | Effect              |
 *     |   0   | Turn off CLKA       |
 *     |   1   | CLKA = PREA         |
 *     | 2-255 | CLKA = PREA / Value |  In other words, CLKA = PREA / DIVA
 *     ===============================
 *
 * @param preA The Prescalar to use for clock A.
 * @param divA The Divider to use for clock A.
 */
void pwmc_configure_clockA(uint8_t preA, uint8_t divA) {

	/* ~ Shift options into proper positions to write to the register. ~ */

	uint16_t clockA_config = divA | (preA << 8);

	/* ~ Write the value for the options into the register. ~ */

	set_bits_in_port_with_mask(AT91C_BASE_PWMC -> PWMC_MR, clockA_config);

}

/**
 * Configures clock B of the PWM Controller.  Configuring the clock
 * involves defining a Prescalar value and a Divider value. The
 * available values are as follows:
 *
 *     Defining Prescalar B (preB):
 *     ====================================
 *     |   Binary   | Hex  |   Value      |
 *     | 0b00000000 | 0x00 | (MCK / 1   ) |
 *     | 0b00000001 | 0x01 | (MCK / 2   ) |
 *     | 0b00000010 | 0x02 | (MCK / 4   ) |
 *     | 0b00000011 | 0x03 | (MCK / 8   ) |
 *     | 0b00000100 | 0x04 | (MCK / 16  ) |
 *     | 0b00000101 | 0x05 | (MCK / 32  ) |
 *     | 0b00000110 | 0x06 | (MCK / 64  ) |
 *     | 0b00000111 | 0x07 | (MCK / 128 ) |
 *     | 0b00001000 | 0x08 | (MCK / 256 ) |
 *     | 0b00001001 | 0x09 | (MCK / 512 ) |
 *     | 0b00001010 | 0x0A | (MCK / 1024) |
 *     ====================================
 *
 *     Defining Divider B (divB):
 *     ===============================
 *     | Value | Effect              |
 *     |   0   | Turn off CLKB       |
 *     |   1   | CLKB = PREB         |
 *     | 2-255 | CLKB = PREB / Value |  In other words, CLKB = PREB / DIVB
 *     ===============================
 *
 * @param preB The Prescalar to use for clock B.
 * @param divB The Divider to use for clock B.
 */
void pwmc_configure_clockB(uint8_t preB, uint8_t divB) {

	/* ~ Shift options into proper positions to write to the register. ~ */

	uint16_t clockB_config = (divB << 16) | (preB << 24);

	/* ~ Write the value for the options into the register. ~ */

	set_bits_in_port_with_mask(AT91C_BASE_PWMC -> PWMC_MR, clockB_config);

}

/**
 * Configures the clock to use for the given PWM channel.
 *
 *     Options for clock include:
 *     ==========================================================
 *     | AT91C_PWMC_CPRE_MCK        Select to use master clock. |
 *     | AT91C_PWMC_CPRE_MCKA       Select to use clock A.      |
 *     | AT91C_PWMC_CPRE_MCKB       Select to use clock B.      |
 *     ==========================================================
 *
 *     Additional options for clock (for manually selecting a divide factor):
 *     ====================================
 *     |   Binary   | Hex  | Function     |
 *     | 0b00000000 | 0x00 | (MCK / 1   ) |  Same as using AT91C_PWMC_CPRE_MCK
 *     | 0b00000001 | 0x01 | (MCK / 2   ) |
 *     | 0b00000010 | 0x02 | (MCK / 4   ) |
 *     | 0b00000011 | 0x03 | (MCK / 8   ) |
 *     | 0b00000100 | 0x04 | (MCK / 16  ) |
 *     | 0b00000101 | 0x05 | (MCK / 32  ) |
 *     | 0b00000110 | 0x06 | (MCK / 64  ) |
 *     | 0b00000111 | 0x07 | (MCK / 128 ) |
 *     | 0b00001000 | 0x08 | (MCK / 256 ) |
 *     | 0b00001001 | 0x09 | (MCK / 512 ) |
 *     | 0b00001010 | 0x0A | (MCK / 1024) |
 *     | 0b00001011 | 0x0B | (   CLKA   ) |  Same as using AT91C_PWMC_CPRE_MCKA
 *     | 0b00001100 | 0x0C | (   CLKB   ) |  Same as using AT91C_PWMC_CPRE_MCKB
 *     ==================================
 *
 * @param pwm The PWM channel to choose a clock for.
 * @param clock The clock configuration to use.
 */
void pwm_set_clock(AT91S_PWMC_CH *pwm, uint8_t clock) {

	/* ~ Set the register's value using the given clock option. ~ */

	set_bits_in_port_with_mask(pwm -> PWMC_CMR, clock);

}

/**
 * Sets the given PWM channel to be a left-aligned waveform.
 */
void pwm_set_left_aligned(AT91S_PWMC_CH *pwm) {

	/* ~ Set the alignment bit to 0 for left-aligned. ~ */

	clear_bit_in_port(8, pwm -> PWMC_CMR);

}

/**
 * Sets the given PWM channel to be a center-aligned waveform.
 * @param pwm The PWM channel to set center-aligned.
 */
void pwm_set_center_aligned(AT91S_PWMC_CH *pwm) {

	/* ~ Set the alignment bit to 1 for center-aligned. ~ */

	set_bit_in_port(8, pwm -> PWMC_CMR);

}

/**
 * Sets the PWM channel's polarity.
 * @param pwm The PWM channel to set the polarity of.
 * @param polarity True causes the waveform to start high, false starts low.
 */
void pwm_set_polarity(AT91S_PWMC_CH *pwm, bool polarity) {

	if(polarity) {

		/* ~ Sets the CPOL to 1 so the output waveform starts high. ~ */

		set_bit_in_port(9, pwm -> PWMC_CMR);

	} else {

		/* ~ Sets the CPOL to 0 so the output waveform starts low. ~ */

		clear_bit_in_port(9, pwm -> PWMC_CMR);

	}
}

void pwm_configure(AT91S_PWMC_CH *pwm) {

	/* ~ Power on the PWM Module. ~ */

	pwmc_power_on();

	/* ~ Set the default clock to the master clock. ~ */

	pwm_set_clock(pwm, AT91C_PWMC_CPRE_MCK);

	/* ~ Sets the default waveform alignment to left-aligned. ~ */

	pwm_set_left_aligned(pwm);

	/* ~ Sets the default waveform polarity to positive. ~ */

	pwm_set_polarity(pwm, true);

	/* ~ Configure the default period for each channel. ~ */
	/* ~ Configure the default duty cycle for each channel. ~ */

}

/**
 * Sets the given PWM channel as enabled.
 * @param channel The PWM channel to enable.
 */
void pwm_enable(uint8_t channel) {

	set_bits_in_port_with_mask(AT91C_BASE_PWMC -> PWMC_ENA, channel);

}

/**
 * Sets the given PWM channel as disabled.
 * @param channel The PWM channel to disable.
 */
void pwm_disable(uint8_t channel) {

	set_bits_in_port_with_mask(AT91C_BASE_PWMC -> PWMC_DIS, channel);

}

/**
 * Sets the given PWM channel to enable interrupts.
 * @param channel The PWM channel to enable interrupts for.
 */
void pwm_enable_interrupt(uint8_t channel) {

	set_bits_in_port_with_mask(AT91C_BASE_PWMC -> PWMC_IER, channel);

}

/**
 * Sets the given PWM channel to disable interrupts.
 * @param channel The PWM channel to disable interrupts for.
 */
void pwm_disable_interrupt(uint8_t channel) {

	set_bits_in_port_with_mask(AT91C_BASE_PWMC -> PWMC_IDR, channel);

}

/**
 * Tells whether the given PWM channel is currently enabled or disabled.
 * @param channel The PWM channel to check.
 * @return True if the given channel is enabled, false if it's disabled.
 */
bool pwm_enabled(uint8_t channel) {

	return get_bits_from_port_with_mask(AT91C_BASE_PWMC -> PWMC_SR, channel);

}

/**
 * Tells whether the given PWM channel currently has interrupts enabled.
 * @param channel The PWM channel to check.
 * @return True if the given channel has interrupts enabled, false if disabled.
 */
bool pwm_interrupt_enabled(uint8_t channel) {

	return get_bits_from_port_with_mask(AT91C_BASE_PWMC -> PWMC_IMR, channel);

}

/**
 * Tells whether a cycle has finished on the given PWM channel since the last
 * check.
 * @param channel The PWM channel to check.
 * @return True if a period cycle has completed, false otherwise.
 */
bool pwm_finished_cycle(uint8_t channel) {

	return get_bits_from_port_with_mask(AT91C_BASE_PWMC -> PWMC_ISR, channel);

}

/* ~ ~------------------------------- PWM0 -------------------------------~ ~ */

void pwm0_configure(void) {

	pwm_configure(AT91C_BASE_PWMC_CH0);

}

void pwm0_set_clock(uint8_t clock) {

	pwm_set_clock(AT91C_BASE_PWMC_CH0, clock);

}

void pwm0_set_left_aligned() {

	pwm_set_left_aligned(AT91C_BASE_PWMC_CH0);

}

void pwm0_set_center_aligned() {

	pwm_set_center_aligned(AT91C_BASE_PWMC_CH0);

}

void pwm0_set_polarity(bool polarity) {

	pwm_set_polarity(AT91C_BASE_PWMC_CH0, polarity);

}

void pwm0_enable() {

	pwm_enable(AT91C_PWMC_CHID0);

}

void pwm0_disable() {

	pwm_disable(AT91C_PWMC_CHID0);

}

void pwm0_enable_interrupt() {

	pwm_enable_interrupt(AT91C_PWMC_CHID0);

}

void pwm0_disable_interrupt() {

	pwm_disable_interrupt(AT91C_PWMC_CHID0);

}

bool pwm0_enabled(void) {

	return pwm_enabled(AT91C_PWMC_CHID0);

}

bool pwm0_interrupt_enabled(void) {

	return pwm_interrupt_enabled(AT91C_PWMC_CHID0);

}

bool pwm0_finished_cycle(void) {

	return pwm_finished_cycle(AT91C_PWMC_CHID0);

}

/* ~ ~------------------------------- PWM1 -------------------------------~ ~ */

void pwm1_configure(void) {

	pwm_configure(AT91C_BASE_PWMC_CH1);

}

void pwm1_set_clock(uint8_t clock) {

	pwm_set_clock(AT91C_BASE_PWMC_CH1, clock);

}

void pwm1_set_left_aligned() {

	pwm_set_left_aligned(AT91C_BASE_PWMC_CH1);

}

void pwm1_set_center_aligned() {

	pwm_set_center_aligned(AT91C_BASE_PWMC_CH1);

}

void pwm1_set_polarity(bool polarity) {

	pwm_set_polarity(AT91C_BASE_PWMC_CH1, polarity);

}

void pwm1_enable() {

	pwm_enable(AT91C_PWMC_CHID1);

}

void pwm1_disable() {

	pwm_disable(AT91C_PWMC_CHID1);

}

void pwm1_enable_interrupt() {

	pwm_enable_interrupt(AT91C_PWMC_CHID1);

}

void pwm1_disable_interrupt() {

	pwm_disable_interrupt(AT91C_PWMC_CHID1);

}

bool pwm1_enabled(void) {

	return pwm_enabled(AT91C_PWMC_CHID1);

}

bool pwm1_interrupt_enabled(void) {

	return pwm_interrupt_enabled(AT91C_PWMC_CHID1);

}

bool pwm1_finished_cycle(void) {

	return pwm_finished_cycle(AT91C_PWMC_CHID1);

}

/* ~ ~------------------------------- PWM2 -------------------------------~ ~ */

void pwm2_configure(void) {

	pwm_configure(AT91C_BASE_PWMC_CH2);

}

void pwm2_set_clock(uint8_t clock) {

	pwm_set_clock(AT91C_BASE_PWMC_CH2, clock);

}

void pwm2_set_left_aligned() {

	pwm_set_left_aligned(AT91C_BASE_PWMC_CH2);

}

void pwm2_set_center_aligned() {

	pwm_set_center_aligned(AT91C_BASE_PWMC_CH2);

}

void pwm2_set_polarity(bool polarity) {

	pwm_set_polarity(AT91C_BASE_PWMC_CH2, polarity);

}

void pwm2_enable() {

	pwm_enable(AT91C_PWMC_CHID2);

}

void pwm2_disable() {

	pwm_disable(AT91C_PWMC_CHID2);

}

void pwm2_enable_interrupt() {

	pwm_enable_interrupt(AT91C_PWMC_CHID2);

}

void pwm2_disable_interrupt() {

	pwm_disable_interrupt(AT91C_PWMC_CHID2);

}

bool pwm2_enabled(void) {

	return pwm_enabled(AT91C_PWMC_CHID2);

}

bool pwm2_interrupt_enabled(void) {

	return pwm_interrupt_enabled(AT91C_PWMC_CHID2);

}

bool pwm2_finished_cycle(void) {

	return pwm_finished_cycle(AT91C_PWMC_CHID2);

}

/* ~ ~------------------------------- PWM3 -------------------------------~ ~ */

void pwm3_configure(void) {

	pwm_configure(AT91C_BASE_PWMC_CH3);

}

void pwm3_set_clock(uint8_t clock) {

	pwm_set_clock(AT91C_BASE_PWMC_CH3, clock);

}

void pwm3_set_left_aligned() {

	pwm_set_left_aligned(AT91C_BASE_PWMC_CH3);

}

void pwm3_set_center_aligned() {

	pwm_set_center_aligned(AT91C_BASE_PWMC_CH3);

}

void pwm3_set_polarity(bool polarity) {

	pwm_set_polarity(AT91C_BASE_PWMC_CH3, polarity);

}

void pwm3_enable() {

	pwm_enable(AT91C_PWMC_CHID3);

}

void pwm3_disable() {

	pwm_disable(AT91C_PWMC_CHID3);

}

void pwm3_enable_interrupt() {

	pwm_enable_interrupt(AT91C_PWMC_CHID3);

}

void pwm3_disable_interrupt() {

	pwm_disable_interrupt(AT91C_PWMC_CHID3);

}

bool pwm3_enabled(void) {

	return pwm_enabled(AT91C_PWMC_CHID3);

}

bool pwm3_interrupt_enabled(void) {

	return pwm_interrupt_enabled(AT91C_PWMC_CHID3);

}

bool pwm3_finished_cycle(void) {

	return pwm_finished_cycle(AT91C_PWMC_CHID3);

}
