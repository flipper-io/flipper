#include <flipper/timer.h>
#include <flipper/error.h>

/* NOTE: TC0 is reserved by the system scheduler. */

/* Expose channel A within TC0. */
TcChannel *TCA = &(TC0->TC_CHANNEL[0]);

/* A data structure to hold information pertainent to each timer. */
struct _lf_timer {
	/* A pointer to the timer channel. */
	TcChannel *CH;
	/* Whether or not the timer is in use. */
	bool available;
	/* The callback function registered to the timer event. */
	void (* callback)(void);
};

/* Initialize an array of all the timers. */
struct _lf_timer timers[] = { { &(TC0->TC_CHANNEL[0]), true, NULL },
							  { &(TC0->TC_CHANNEL[1]), true, NULL },
							  { &(TC0->TC_CHANNEL[2]), true, NULL },
							  { &(TC0->TC_CHANNEL[3]), true, NULL } };

int timer_configure(void) {
	/* Iterate through the timers and configure their defaults. */
	for (size_t i = 0; i < sizeof(timers)/sizeof(struct _lf_timer); i ++) {
		/* Enable the timer's peripheral clock. */
		PMC->PMC_PCER0 |= (1 << (ID_TC0 + i));
		/* Disable the source clock to TCA. */
		timers[i].CH->TC_CCR = TC_CCR_CLKDIS;
		/* Select the incoming clock signal as MCK / 128. */
		timers[i].CH->TC_CMR = TC_CMR_TCCLKS_TIMER_CLOCK4;
		/* Enable the TCA C compare interrupt. */
		timers[i].CH->TC_IER = TC_IER_CPCS;
		/* Set the compare value into C. */
		timers[i].CH->TC_RC = 0xFFFF;
		/* Enable the timer's interrupt. */
		NVIC_EnableIRQ(TC0_IRQn + i);
	}
	return lf_success;
}

/* Registers a callback with the next available timer. */
int timer_register(uint32_t ticks, void *callback) {
	/* Loop through the timers until a free timer is found. */
	for (size_t i = 0; i < sizeof(timers); i ++) {
		if (timers[i].available) {
			/* Hold the timer. */
			timers[i].available = false;
			/* Set the callback address. */
			timers[i].callback = callback;
			/* Set the compare value into C. */
			timers[i].CH->TC_RC = ticks;
			/* Enable the clock and start the timer. */
			timers[i].CH->TC_CCR = TC_CCR_CLKEN | TC_CCR_SWTRG;
			return lf_success;
		}
	}
	lf_error_raise(E_TIMER, error_message("No timer is available to register a callback."));
	return lf_error;
}

void tcx_isr(uint8_t timer) {
	/* Read the interrupt flag to clear it. */
	timers[timer].CH->TC_SR;
	/* Perform the timer callback. */
	timers[timer].callback();
	/* Stop the timer. */
	timers[timer].CH->TC_CCR = TC_CCR_CLKDIS;
	/* Release the timer. */
	timers[timer].available = true;
}

void tc0_isr(void) {
	tcx_isr(0);
}

void tc1_isr(void) {
	tcx_isr(1);
}

void tc2_isr(void) {
	tcx_isr(2);
}

void tc3_isr(void) {
	tcx_isr(3);
}

void tc4_isr(void) {
	tcx_isr(4);
}

void tc5_isr(void) {
	tcx_isr(5);
}
