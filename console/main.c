#define __private_include__

#include <flipper.h>

#include <fs/tree.h>

int main(int argc, char *argv[]) {
	
	flipper.attach(FLIPPER_SOURCE_FVM, "/Development/flipper-toolbox/fvm/hal.fvm");
	
	led.rgb(0, 0, 0);
	
	return 0;
	
}