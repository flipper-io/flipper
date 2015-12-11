#define __private_include__

#include <sam/sam.h>

#include <fmr/fmr.h>

/* ~ This function configures the main processor. ~ */

void sam_configure(void) {
	
	
	
}

/* ~ This function turns the power to the main processor on or off. ~ */

void sam_set_power(bool power) {

	device.invoke(_sam, _sam_set_power, 1, power);
	
}

/* ~ This function issues a hardware reset to the main processor. ~ */

void sam_reset(void) {

	device.invoke(_sam, _sam_reset, 0, NO_ARGS);

}

/* ~ This function puts the main processor into DFU mode. ~ */

void sam_load_dfu(void) {
	
	device.invoke(_sam, _sam_load_dfu, 0, NO_ARGS);
	
}

/* ~ This function completely erases the main processor's at45 memory. ~ */

void sam_format(void) {
	
	device.invoke(_sam, _sam_format, 0, NO_ARGS);
	
}