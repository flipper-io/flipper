#define __private_include__
#include <flipper.h>
#include <platform/posix.h>
#include <platform/fvm.h>

int main(int argc, char *argv[]) {

	flipper.attach();

	printf(KGRN "Successfully attached to Flipper device.\n");

	while(1) {
		while(!uart.ready());
		uint8_t c = uart.get();
		printf("0x%02x (%c)\n", c, c);
	}

	return EXIT_SUCCESS;
}
