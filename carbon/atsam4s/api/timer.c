#include <flipper.h>
#include "timer.h"
#include <tc/tc.h>

LF_FUNC("timer") int timer_configure(void) {
	return lf_success;
}

/* Registers a callback with the next available timer. */
LF_FUNC("timer") int timer_register(uint32_t ticks, void *callback) {
	return lf_error;
}

void tcx_isr(uint8_t timer) {

}

/* timer0 isr */
void tc0_isr(void) {
	tcx_isr(0);
}

/* timer1 isr */
void tc1_isr(void) {
	tcx_isr(1);
}

/* timer2 isr */
void tc2_isr(void) {
	tcx_isr(2);
}

/* timer3 isr */
void tc3_isr(void) {
	tcx_isr(3);
}

/* timer4 isr */
void tc4_isr(void) {
	tcx_isr(4);
}

/* timer5 isr */
void tc5_isr(void) {
	tcx_isr(5);
}
