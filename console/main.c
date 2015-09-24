#define __private_include__

#include <flipper.h>



#include <fmr/fmr.h>

int main(int argc, char *argv[]) {
	
	// if (argc < 2) { printf("Welcome to the Flipper Console.\n\n"); }
	
	//flipper.attach(FLIPPER_SOURCE_NETWORK, "129.21.82.216");
	
	//flipper.attach(FLIPPER_SOURCE_USB);
	
	/* ~ Attatch this instance of libflipper to the Flipper Virtual Machine for verbose debugging output. ~ */
	
	flipper.attach(FLIPPER_SOURCE_FVM);
		
	led.rgb(0xFA, 0xFB, 0xFC);
	
	printf("\n\n");
	
	
}