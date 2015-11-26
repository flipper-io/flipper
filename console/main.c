#define __private_include__

#include "console.h"

#include <fmr/fmr.h>

#include <platform/fmr.h>

int main(int argc, char *argv[]) {
    
	/* ~ Attatch this instance of libflipper to the first device present over USB. ~ */

	flipper.attach(FLIPPER_SOURCE_USB);

	if (!strcmp(argv[1], "flash")) {

		sam_load_firmware(argv[2]);

	}
	
	else if (!strcmp(argv[1], "format")) {
		
		sam.format();
		
	}
    
    else if (!strcmp(argv[1], "io") && !strcmp("direction", argv[2])) {
        host.invoke(_io, _io_set_direction, 4, little(atoi(argv[3])), 0, little(OUTPUT), 0);
        
    }
    
    else if (!strcmp(argv[1], "io") && !strcmp("write", argv[2])) {
        
        host.invoke(_io, _io_write, 4, little(atoi(argv[3])), 0, little(atoi(argv[4])), 0);
        
    }
    
	return 0;

}
