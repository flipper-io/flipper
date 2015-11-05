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

	flipper.attatch(FLIPPER_SOURCE_USB);

	/* ~ Attatch this instance of libflipper to a hardware abstraction layer. ~ */

	flipper.attach(FLIPPER_SOURCE_FVM, "/Development/flipper-toolbox/fvm/hal.fvm");

	int args[] = { 1, 2, 3 };

	internal_call(&whatnot, 3, args);

	return 0;

}
