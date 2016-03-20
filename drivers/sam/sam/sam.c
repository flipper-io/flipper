#define __private_include__
#include <sam/sam.h>
#include <fmr/fmr.h>

void sam_configure(void) {

}

void sam_set_power(bool power) {

	device.invoke(_sam, _sam_set_power, 1, power);

}

void sam_reset(void) {

	device.invoke(_sam, _sam_reset, 0, NO_ARGS);

}

void sam_load_dfu(void) {

	device.invoke(_sam, _sam_load_dfu, 0, NO_ARGS);

}

void sam_format(void) {

	device.invoke(_sam, _sam_format, 0, NO_ARGS);

}

void sam_suspend(void) {

	device.invoke(_sam, _sam_suspend, 0, NO_ARGS);

}

void sam_engage(void) {

	device.invoke(_sam, _sam_engage, 0, NO_ARGS);

}
