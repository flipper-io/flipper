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
	
	if (!strcmp(argv[1], "word")) {
		
		void *address = (void *)(strtol(argv[2], NULL, 16));
		
		printf("\nRead from address 0x%08x: 0x%08x\n\n", (uint32_t)address, sam.word(address));
		
	}
	
    else if (!strcmp(argv[1], "load")) {
		
        /* ~ Parse user input from the variadic argument list. ~ */
        
        char *bid = argv[2], *path = argv[3];
        
        /* ~ Open the file. ~ */
        
        FILE *file = fopen (path, "r");
		
		/* ~ Ensure the file opening process was error free. ~ */
        
		if (!file) { printf("\nCould not open the file: %s\n\n", path); return 0; }
		
        /* ~ Obtain the size of the file. ~ */
        
        fseek(file, 0L, SEEK_END);
        
        uint32_t size = (uint32_t)(ftell(file));
        
        fseek(file, 0L, SEEK_SET);
        
        /* ~ Load the file into local memory. ~ */
        
        uint8_t *data = (uint8_t *) malloc(sizeof(uint8_t) * size);
        
        fread(data, size, sizeof(uint8_t), file);
        
        /* ~ Checksum the bundle identifier to obtain a key. ~ */
        
        uint16_t key = checksum(bid, strlen(bid));
		
        /* ~ Create an new entry in the filesystem. ~ */
        
        fsp _leaf = fs_add_leaf_with_key(_root_leaf, key);
        
        /* ~ Allocate space in external memory for the data. ~ */
        
        fsp _data = at45_alloc(size);
        
        /* ~ Move the data into external flash. ~ */
        
        for (int i = 0; i < (size / 64); i ++) at45_push((void *)(data + (64 * i)), 64, (fsp)(_data + (64 * i)));
        
        at45_push((void *)(data + (64 * (size / 64))), (size % 64), (fsp)(_data + (64 * (size / 64))));
        
        /* ~ Rewrite the pointers. ~ */
        
        at45_push(&_data, sizeof(fsp), forward(_leaf, leaf, data));

        at45_push(&size, sizeof(uint32_t), forward(_leaf, leaf, size));
		
        /* ~ Call the the Flipper Dynamic Loader to begin the dynamic loading process. ~ */
        
        void *address = fdl.load(key);
		
		if (address) {
			
			/* ~ Display a message indicating that the loading process was completed successfuly. ~ */
			
			printf("\nDynamic loading completed successfully. %p.\n\n", address);
			
		}
		
		else {
			
			printf("\nDynamic loading failed.\n\n");
			
		}
		
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
		
		sam_reset();
		
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
