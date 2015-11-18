#define __private_include__

#include "console.h"

#define UPLOADER

int main(int argc, char *argv[]) {

    flipper.attach(FLIPPER_SOURCE_USB);
    
    led.rgb(0, 10, 0);
    
#ifndef UPLOADER
	
	
	
#else
	
	/* ~ Attatch this instance of libflipper to the first device present over USB. ~ */

	flipper.attach(FLIPPER_SOURCE_USB);

	if (!strcmp(argv[1], "flash")) {

		sam_load_firmware(argv[2]);

	}
	
	else if (!strcmp(argv[1], "format")) {
		
		sam.format();
		
	}
	
#endif
	
	return 0;

}
