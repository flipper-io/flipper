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
	flipper.attach(FLIPPER_USB, "elroy");

	/* ~ Change the LED color. ~ */
	led.rgb(0, 0, 25);

	return 0;

}
