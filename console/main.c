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
        
        sam.power(false);
		
		uint32_t _key = 0;
		
        fs_format();
		
		at45_push(&_key, sizeof(uint32_t), config_offset(FDL_CONFIG_BASE, FDL_LOADED_KEY));
		
        /* ~ Get user input. ~ */
        
        char *name = argv[2];
        
        char *path = argv[3];
        
        /* ~ Open the file. ~ */
        
        FILE *file = fopen (path, "r");
        
        if (!file) { printf("\nCould not open the file: %s\n\n", path); exit(EXIT_FAILURE); }
        
        /* ~ Obtain the size of the file. ~ */
        
        fseek(file, 0L, SEEK_END);
        
        uint32_t size = (uint32_t)(ftell(file));
        
        fseek(file, 0L, SEEK_SET);
        
        /* ~ Load the file into memory. ~ */
        
        uint8_t *data = (uint8_t *) malloc (sizeof(uint8_t) * size);
        
        fread(data, size, sizeof(uint8_t), file);
        
        /* ~ Checksum the name to obtain a key. ~ */
        
        uint16_t key = checksum(name, strlen(name));
		
		printf("\nLoading with key 0x%04x\n", key);
        
        /* ~ Create an entry in the filesystem. ~ */
        
        fsp _leaf = fs_add_leaf_with_key(_root_leaf, key);
        
        /* ~ Allocate space for the data. ~ */
        
        fsp _data = at45_alloc(size);
        
        /* ~ Move the data into at45. ~ */
        
        for (int i = 0; i < (size / 64); i ++) {
            
            at45_push((void *)(data + (64 * i)), 64, (fsp)(_data + (64 * i)));
            
        }
        
        at45_push((void *)(data + (64 * (size / 64))), (size % 64), (fsp)(_data + (64 * (size / 64))));
        
        /* ~ Rewrite the pointers. ~ */
        
        at45_push(&_data, sizeof(fsp), forward(_leaf, leaf, data));

        at45_push(&size, sizeof(uint32_t), forward(_leaf, leaf, size));
        
        sam.power(true);
        
        usleep(10000);
        
        /* ~ Load the data. ~ */
        
        fdl.load(key);
        
        printf("\nDynamic loading complete.\n\n");
        
    }
	
	else if (!strcmp(argv[1], "unload")) {
		
		uint32_t _key = 0;
		
		/* ~ Clear the loaded key. ~ */
		
		at45_push(&_key, sizeof(uint32_t), config_offset(FDL_CONFIG_BASE, FDL_LOADED_KEY));
		
		/* ~ Clear the loaded key. ~ */
		
		at45_push(&_key, sizeof(uint32_t), config_offset(FDL_CONFIG_BASE, FDL_STARTUP_PROGRAM));
		
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
