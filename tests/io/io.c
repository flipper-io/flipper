/*
 *
 * Tests for the 'io' module.
 *
 */

#include <stdio.h>
#include <flipper.h>

#define DISCLOSE_ERRORS

int main(int argc, char *argv[]) {

	/* ~ Attach a Flipper device over USB. ~ */
	flipper.attach(FLIPPER_SOURCE_USB);

	io.direction(7, OUTPUT);

	return 0;

}
