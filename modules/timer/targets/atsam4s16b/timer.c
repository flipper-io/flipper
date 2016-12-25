#define __private_include__
#include <flipper/timer.h>

/* Expose channel A within TC0. */
TcChannel *TCA = &(TC0 -> TC_CHANNEL[0]);

int timer_configure(void) {
	/* Enable the TC0 peripheral clock. */
	PMC -> PMC_PCER0 |= (1 << ID_TC0);
	/* Disable the source clock to TCA. */
	TCA -> TC_CCR = TC_CCR_CLKDIS;
	/* Select the incoming clock signal as MCK / 128. */
	TCA -> TC_CMR = TC_CMR_TCCLKS_TIMER_CLOCK4;
	/* Enable the TCA C compare interrupt. */
	TCA -> TC_IER = TC_IER_CPCS;
	/* Set the compare value into C. */
	TCA -> TC_RC = 0xFFFF;
	/* Enable the TC0 interrupt. */
	NVIC_EnableIRQ(TC0_IRQn);
	/* Enable the clock and start the timer.*/
	TCA -> TC_CCR = TC_CCR_CLKEN | TC_CCR_SWTRG;
	return lf_success;
}

void tc0_isr(void) {
	/* Read the interrupt flag to clear it. */
	TCA -> TC_SR;
}
