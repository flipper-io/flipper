#include <app.h>

LF_MODULE(_app, "app", "DESCRIPTION");

const struct _app app = {
	&my_func
};

int app_configure(void) {
	return lf_bind(&_app);
}

int my_func(uint8_t a) {
	return lf_invoke(&_app, _app_my_func, fmr_args(fmr_infer(a)));
}

