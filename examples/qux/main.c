#include <stdio.h>
#include <string.h>
#include <flipper.h>
#include <qux.h>

/* ~ Simple program that uses the Flipper module 'qux.' ~ */
int main(int argc, char *argv[]) {

	if (argc < 2) {
		printf("\nUsage: qux [on | off]\n\n");
		return 0;
	}

	/* ~ Attach to the first available Flipper device over USB. ~ */
	flipper.attach();

	/* ~ Configure the 'qux' module. ~ */
	qux.configure();

	/* ~ Call the qux_on function if the first argument is "on". ~ */
	if (!strcmp(argv[1], "on")) {
		qux.on();
	}

	/* ~ Otherwise, call the qux_off function if the first argument is "off". ~ */
	else if (!strcmp(argv[1], "off")) {
		qux.off();
	}

	return 0;

}
