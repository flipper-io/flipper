#define __private_include__

#include <flipper/flipper.h>

#include <fmr/fmr.h>

int main(int argc, char *argv[]) {
	
	printf("Welcome to the Flipper Console!\n\n");
	
	device.invoke(_button, _button_read, 3, 1, 2, 3);
	
	return EXIT_SUCCESS;
	
}