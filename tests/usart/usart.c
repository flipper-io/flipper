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

	/* ~ Send a trivial string over USART. ~ */
	char *hello = "Hello world!";
	usart1.push(hello, strlen(hello));

	return 0;

}
