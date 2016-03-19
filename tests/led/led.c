/*
 *
 * Tests for the 'error' module.
 *
 */

#include <stdio.h>
#include <flipper.h>

#define DISCLOSE_ERRORS

int main(int argc, char *argv[]) {

	/* ~ Attach a Flipper device over USB. ~ */
	flipper.attach(FLIPPER_SOURCE_USB);

	/* ~ Change the LED color. ~ */
	led.rgb(0, 0, 100);

	return 0;

}
