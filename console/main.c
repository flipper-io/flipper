#define __private_include__

#include <flipper.h>

#include <fs/fs.h>

#include <fs/tree.h>

#include <fmr/fmr.h>

uint32_t whatnot(int a, int b, int c) {

	printf("%u, %u, %u\n\n", a, b, c);

	return 0xdeadbeef;

}

int main(int argc, char *argv[]) {

	/* ~ Attatch this instance of libflipper to the first device present over USB. ~ */

	flipper.attach(FLIPPER_SOURCE_USB);

	if (!strcmp(argv[1], "flash")) {

		sam_load_firmware(argv[2]);

	}

	return 0;

}
