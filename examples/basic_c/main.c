#include <stdio.h>
#include <flipper.h>

int main(int argc, char *argv[]) {

	if (argc < 3) { printf("Please provide an IO pin and value.\n"); return 0; }

	/* ~ Attach a Flipper device over USB. ~ */
	flipper.attach(FLIPPER_SOURCE_USB);

	/* ~ Toggle the IO pin on and off. ~ */
	io.direction(atoi(argv[1]), 1);
	io.write(atoi(argv[1]), atoi(argv[2]));

	return 0;

}
