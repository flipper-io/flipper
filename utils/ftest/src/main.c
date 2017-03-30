#define __private_include__
#include <flipper.h>
#include <flipper/platforms/posix.h>

int main(int argc, char *argv[]) {

	flipper.attach();
	led.rgb(0, 0, 100);
	button.read();

	return EXIT_SUCCESS;
}
