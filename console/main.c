#define __private_include__

#include "console.h"

#include <fmr/fmr.h>

#include <platform/fmr.h>

#include <fs/tree.h>

#include <fs/crc.h>

#include <unistd.h>

int main(int argc, char *argv[]) {
    
	if (argc < 2) {

		printf("\nUsage: flipper [load | io | flash]\n\n");

		return EXIT_SUCCESS;

	}

	/* ~ Attatch this instance of libflipper to the first device present over USB. ~ */
	
	flipper.attach(FLIPPER_SOURCE_USB);
	
	if (!strcmp(argv[1], "flash")) {

		sam_load_firmware(argv[2]);

	}
	
	if (!strcmp(argv[1], "load")) {
		
        /* ~ Parse user input from the variadic argument list. ~ */
        
        char *bid = argv[2], *path = argv[3];
		
		sam.power(0);
		
		fs_format();
		
		/* ~ Send the file to the device. ~ */
		
		fs_transfer_file(path, bid);
		
		sam.power(1);
		
		/* ~ Let the operating system boot. ~ */
		
		usleep(10000);
		
		uint16_t key = checksum(bid, strlen(bid));
		
		printf("\nKey: 0x%04x", key);
		
        /* ~ Call the the Flipper Dynamic Loader to begin the dynamic loading process. ~ */
        
        fdl.launch(key);
		
//		if (address) {
//			
//			/* ~ Display a message indicating that the loading process was completed successfuly. ~ */
//			
//			printf("\nDynamic loading completed successfully. %p.\n\n", address);
//			
//		}
//		
//		else {
//			
//			printf("\nDynamic loading failed.\n\n");
//			
//		}
		
    }
	
	else if (!strcmp(argv[1], "boot")) {

		char *bid = argv[2];
		
		uint16_t key = checksum(bid, strlen(bid));

		/* ~ Launch the program. ~ */
		
		fdl.launch(key);
		
	}
	
	else if (!strcmp(argv[1], "unload")) {
		
		uint16_t zero = 0;
		
		/* ~ Zero the FDL break value. ~ */
		
		fdl_write_config(zero, fdl_config_brk);
		
		/* ~ Reconfigure the FDL. ~ */
		
		fdl.configure();
		
	}
	
	else if (!strcmp(argv[1], "reset")) {
		
		sam.suspend();
		
		sam.engage();
		
	}
	
	else if (!strcmp(argv[1], "suspend")) {
		
		sam.suspend();
		
	}
	
	else if (!strcmp(argv[1], "engage")) {
		
		sam.engage();
		
	}
	
	else if (!strcmp(argv[1], "format")) {
		
        fs_format();
		
	}
    
    else if (!strcmp(argv[1], "io") && !strcmp("direction", argv[2])) {
		
        io.direction(atoi(argv[3]), atoi(argv[4]));
		
        
    }
    
    else if (!strcmp(argv[1], "io") && !strcmp("write", argv[2])) {
        
        host.invoke(_io, _io_write, 4, little(atoi(argv[3])), 0, little(atoi(argv[4])), 0);
        
    }
	
	return 0;

}
