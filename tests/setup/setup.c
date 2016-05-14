/*
 *
 * Tests for the setup process of a Flipper device.
 *
 */

#include <stdio.h>
#include <flipper.h>

int main(int argc, char *argv[]) {

	if (argc < 2) { printf("Usage: setup restore\n"); return 0; }

	/* Attach a Flipper device over USB. */
	flipper.attach(FLIPPER_SOURCE_USB);

	/* Extract the name from the parameter list. */
	char *name = argv[1];

	/* Generate an identifier for the new device using the name as a seed. */
	uintcrc_t identifier = checksum(name, strlen(name));

	/* Save the identifier into EEPROM. */
	flipper.defaults(FLIPPER_NAME, identifier);

	/* Set the configuration bit in EEPROM. */
	flipper.set_config();

	return 0;

}
