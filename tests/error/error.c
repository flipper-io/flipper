/*
 *
 * Tests for the 'error' module.
 *
 */

#include <stdio.h>
#include <flipper.h>

#define DISCLOSE_ERRORS

int main(int argc, char *argv[]) {

#ifdef DISCLOSE_ERRORS
	/* If 'DISCLOSE_ERRORS' has been defined, release all error handling responsibilities to the user. */
	error.disclose();
#endif

	if (!flipper.attach(FLIPPER_SOURCE_USB)) {
		/* Since errors have been disclosed, this print statement is executed. */
		printf("Failed to attach a Flipper device. Reason: %s", error.string());
	}

	/* If errors have been disclosed, this part of the program will be reached, regardless whether we have successfully attached a device. */
	/* If errors have not been disclosed, this part of the program will not be reached, as libflipper will handle the error for us. */

	return 0;

}
