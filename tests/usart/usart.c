/*
 *
 * Tests for the 'usart' module.
 *
 */

#include <stdio.h>
#include <string.h>
#include <flipper.h>

int main(int argc, char *argv[]) {

	/* ~ Attach a Flipper device over USB. ~ */
	flipper.attach(FLIPPER_SOURCE_USB);

	error.disclose();

	/* ~ Send a trivial string over USART. ~ */
	char *hello = "ABCDEFGHIJK";
	usart1.push(hello, strlen(hello));

	printf("Error code: %i\n", error.get());

	//printf("\n\nGot character: '%c'\n\n", usart1.get());

	return 0;

}
