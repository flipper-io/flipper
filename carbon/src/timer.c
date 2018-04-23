#include <flipper.h>

enum { _timer_register, _timer_configure };

int timer_register(uint32_t ticks, void* callback);
int timer_configure(void);

void *timer_interface[] = {
	&timer_register,
	&timer_configure
};

LF_MODULE(timer, "timer", timer_interface);

LF_WEAK int timer_register(uint32_t ticks, void* callback) {
	lf_return_t retval;
	lf_invoke(lf_get_current_device(), "timer", _timer_register, lf_int_t, &retval, lf_args(lf_infer(ticks), lf_infer(callback)));
	return (int)retval;
}

LF_WEAK int timer_configure(void) {
	lf_return_t retval;
	lf_invoke(lf_get_current_device(), "timer", _timer_configure, lf_int_t, &retval, NULL);
	return (int)retval;
}

